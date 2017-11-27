{-|
Module      : Craeft.Parser
Description : A Parsec-based parser for Craeft.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Craeft.Parser ( parseExpression
                     , parseType
                     , parseStatement
                     , parseTopLevel
                     , parseProgram ) where

import           Control.Lens

import           Control.Monad.Trans.Except
import           Text.Parsec hiding ( SourcePos )
import           Text.Parsec.String (Parser)
import           Text.Parsec.Error as E
import           Text.Parsec.Expr

import qualified Craeft.AST as AST
import qualified Craeft.Lexer as Lexer
import           Craeft.Utility

--
-- Nicely wrapping everything.
--

parsecToError :: ParseError -> Error
parsecToError e = ParseError (render $ E.errorMessages e) (E.errorPos e)
  where render = showErrorMessages "or" "unknown" "    expecting"
                                   "    unexpected" "end-of-input"

craeftParse :: Parser a -> String -> String -> CraeftExcept a
craeftParse p f s =
  case Text.Parsec.parse (Lexer.startLexer >> p) f s of
    Left err     -> throwE $ parsecToError err
    Right result -> return result

annotate :: Parser a -> Parser (Annotated a)
annotate p = flip Annotated <$> getPosition <*> p

type ParseFn a = String -> String -> CraeftExcept a

parseExpression :: ParseFn (Annotated AST.Expression)
parseExpression = craeftParse expr

parseType :: ParseFn (Annotated AST.Type)
parseType = craeftParse typeParser

parseStatement :: ParseFn (Annotated AST.Statement)
parseStatement = craeftParse $ annotate statement

parseTopLevel :: ParseFn (Annotated AST.TopLevel)
parseTopLevel = craeftParse $ annotate topLevel

parseProgram :: ParseFn AST.Program
parseProgram = craeftParse $ many (annotate topLevel) <* eof

--
-- Parsing expressions.
--

type ExpressionParser = Parser (Annotated AST.Expression)

positionExpr :: (SourcePos -> ExpressionParser) -> ExpressionParser
positionExpr f = do pos <- getPosition
                    f pos

unsigned pos = do i <- Lexer.unsigned
                  return $ Annotated (AST.UIntLiteral i) pos
  <?> "unsigned integer literal"
signed pos = do i <- Lexer.integer
                return $ Annotated (AST.IntLiteral i) pos
  <?> "signed integer literal"
float pos = do f <- Lexer.float
               return $ Annotated (AST.FloatLiteral f) pos
  <?> "float literal"
string pos = do s <- Lexer.string
                return $ Annotated (AST.StringLiteral s) pos
  <?> "string literal"

cast pos = do t <- Lexer.parens typeParser
              e <- term
              return $ Annotated (AST.Cast t e) pos
  <?> "cast"


expr :: ExpressionParser
expr = buildExpressionParser table term <?> "expression"

term :: ExpressionParser
term = do p <- getPosition
          try (positionExpr cast)
            <|> Lexer.parens expr
            <|> positionExpr unsigned
            <|> positionExpr signed
            <|> positionExpr float
            <|> positionExpr Craeft.Parser.string
            <|> positionExpr functionCall
            <|> do lv <- AST.LValueExpr <$> (dereference <|> variable)
                   return $ Annotated lv p
            <?> "simple expression"

functionCall :: SourcePos -> ExpressionParser
functionCall pos = flip Annotated pos <$>
    do fname <- AST.LValueExpr . AST.Variable <$> Lexer.identifier
       (do targs <- (do Lexer.openTemplate
                        targs <- Lexer.commaSep typeParser
                        Lexer.closeTemplate
                        return targs) <|> return []
           args <- Lexer.parens $ Lexer.commaSep expr
           return $ AST.FunctionCall (Annotated fname pos) args targs)
          <|> return fname

lvalue :: Parser AST.LValue
lvalue = dereference <|> variable <|> fieldAccess <?> "lvalue"

variable :: Parser AST.LValue
variable = AST.Variable <$> Lexer.identifier <?> "variable"

fieldAccess :: Parser AST.LValue
fieldAccess = do e <- expr
                 Lexer.dot
                 (AST.Variable v) <- variable
                 return $ AST.FieldAccess (e ^. contents) v

dereference :: Parser AST.LValue
dereference = (do Lexer.reservedOp "*"
                  AST.Dereference . view contents <$> expr) <?> "dereference"

fieldAccessOp = Infix ((do pos <- getPosition
                           Lexer.reservedOp "."
                           return $ result pos) <?> "field access")
                      AssocLeft
  -- This can lead to a hard crash.  Shh...
  where result pos (Annotated struct _)
                   (Annotated (AST.LValueExpr (AST.Variable v)) _)
          = Annotated (AST.LValueExpr (AST.FieldAccess struct v)) pos
        result _ _ _ = undefined

binary op = Infix (do pos <- getPosition
                      Lexer.reservedOp op
                      let f x y = Annotated (AST.Binop x op y) pos
                      return f) AssocLeft

table = [fieldAccessOp] : (fmap binary <$>
        [ ["*", "/"]
        , ["+", "-"]
        , ["<", "<=", ">", ">="]
        , ["==", "!="]
        , ["&&"], ["||"] ])

-- Parsing types.

typeParser :: Parser (Annotated AST.Type)
typeParser = do p <- getPosition
                n <- Lexer.tname
                let name = Annotated (AST.NamedType n) p
                ptrs <- map _pos <$> many (annotate $ Lexer.reservedOp "*")
                let aux pos t = Annotated (AST.Pointer t) pos
                return $ foldr aux name ptrs

-- Parsing statements.
statement :: Parser AST.Statement
statement = compoundDeclaration
        <|> AST.ExpressionStatement <$> expr
        <|> assignment
        <|> returnStatement
        <|> ifStatement
        <?> "statement"

returnStatement :: Parser AST.Statement
returnStatement = (do Lexer.reserved "return"
                      (AST.Return <$> expr) <|> return AST.VoidReturn)
              <?> "return statement"

assignment :: Parser AST.Statement
assignment = do p <- getPosition
                l <- lvalue
                ((do Lexer.equals
                     r <- expr
                     return $ AST.Assignment l r) <?> "assignment")
                   <|> let e = Annotated (AST.LValueExpr l) p
                        in return $ AST.ExpressionStatement e

declaration :: Parser AST.ValueDeclaration
declaration = do t <- typeParser
                 AST.Variable v <- variable
                 return $ AST.ValueDeclaration (t ^. contents) v

compoundDeclaration :: Parser AST.Statement
compoundDeclaration = do d <- annotate declaration
                         (do Lexer.equals
                             e <- expr
                             return $ AST.CompoundDeclaration d e)
                           <|> return (AST.Declaration $ d ^. contents)

semiFollowed :: Parser a -> Parser [a]
semiFollowed p = many $ do s <- p
                           Lexer.semi 
                           return s

block :: Parser AST.Block
block = semiFollowed $ annotate statement

ifStatement :: Parser AST.Statement
ifStatement = do Lexer.reserved "if"
                 cond <- expr
                 ifBlock <- Lexer.braces block
                 elseBlock <- (do try $ Lexer.reserved "else"
                                  Lexer.braces block) <|> return []
                 return $ AST.IfStatement cond ifBlock elseBlock

--
-- Top-level parsing.
--

topLevel :: Parser AST.TopLevel
topLevel = structDeclaration <|> functionDefinition <?> "toplevel form"

structDeclaration :: Parser AST.TopLevel
structDeclaration = (do Lexer.reserved "struct"
                        name <- Lexer.tname
                        (do fields <- Lexer.braces members
                            return $ AST.StructDeclaration name fields)
                           <|> return (AST.TypeDeclaration name))
                      <?> "struct definition"
  where members = semiFollowed $ annotate declaration

functionSignature :: Parser AST.FunctionSignature
functionSignature = (do
      Lexer.reserved "fn"
      targs <- (do Lexer.openTemplate
                   targs <- Lexer.commaSep (annotate Lexer.tname)
                   Lexer.closeTemplate
                   return targs) <|> return []
      name <- Lexer.identifier
      args <- argList
      let void = Annotated AST.Void <$> getPosition
      ret <- (Lexer.arrow >> typeParser) <|> void
      return $ AST.FunctionSignature name args targs ret)
    <?> "function signature"
  where argList = Lexer.parens $ Lexer.commaSep (annotate declaration)

functionDefinition :: Parser AST.TopLevel
functionDefinition = annotate functionSignature >>= fnDeclOrDef

fnDeclOrDef :: Annotated AST.FunctionSignature -> Parser AST.TopLevel
fnDeclOrDef sig = (AST.FunctionDefinition sig <$> Lexer.braces block
                    <?> "function definition")
                <|> (Lexer.semi >> return (AST.FunctionDecl $ sig ^. contents))
