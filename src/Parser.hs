module Parser ( parseExpression
              , parseType
              , parseStatement
              , parseTopLevel
              , parseProgram ) where

import Control.Monad
import Control.Monad.Trans.Except
import Text.Parsec hiding ( SourcePos )
import Text.Parsec.String (Parser)
import Text.Parsec.Error as E
import Text.Parsec.Expr
import Text.Parsec.Char
import qualified AST
import qualified Lexer
import Error

--
-- Nicely wrapping everything.
--

parsecToError :: ParseError -> Error
parsecToError e = ParseError (render $ E.errorMessages e) (E.errorPos e)
  where render = showErrorMessages "or" "unknown" "    expecting"
                                   "    unexpected" "end-of-input"

craeftParse :: Parser a -> String -> String -> CraeftExcept a
craeftParse p f s =
  case Text.Parsec.parse p f s of
    Left err     -> throwE $ parsecToError err
    Right result -> return result

annotate :: Parser a -> Parser (Annotated a)
annotate p = flip Annotated <$> getPosition <*> p

type ParseFn a = String -> String -> CraeftExcept a

parseExpression :: ParseFn AST.PositionedExpression
parseExpression = craeftParse expr

parseType :: ParseFn (Annotated AST.Type)
parseType = craeftParse typeParser

parseStatement :: ParseFn (Annotated AST.PositionedStatement)
parseStatement = craeftParse $ annotate statement

parseTopLevel :: ParseFn (Annotated AST.PositionedTopLevel)
parseTopLevel = craeftParse $ annotate topLevel

parseProgram :: ParseFn AST.Program
parseProgram = craeftParse $ many $ annotate topLevel

--
-- Parsing expressions.
--

type ExpressionParser = Parser AST.PositionedExpression

positionExpr :: (SourcePos -> ExpressionParser) -> ExpressionParser
positionExpr f = do pos <- getPosition
                    f pos

unsigned pos = do i <- Lexer.unsigned
                  return $ AST.EW (AST.UIntLiteral i) pos
signed pos = do i <- Lexer.integer
                return $ AST.EW (AST.IntLiteral i) pos
float pos = do f <- Lexer.float
               return $ AST.EW (AST.FloatLiteral f) pos
string pos = do s <- Lexer.string
                return $ AST.EW (AST.StringLiteral s) pos

expr = buildExpressionParser table term <?> "expression"

term :: ExpressionParser
term = do p <- getPosition
          Lexer.parens expr
            <|> positionExpr unsigned
            <|> positionExpr signed
            <|> positionExpr float
            <|> positionExpr Parser.string
            <|> do lv <- AST.LValueExpr <$> (dereference <|> variable)
                   return $ AST.EW lv p
            <?> "simple expression"

lvalue :: Parser AST.PositionedLValue
lvalue = dereference <|> variable <|> fieldAccess <?> "lvalue"

variable :: Parser AST.PositionedLValue
variable = AST.Variable <$> Lexer.identifier <?> "variable"

fieldAccess :: Parser AST.PositionedLValue
fieldAccess = do e <- expr
                 Lexer.dot
                 (AST.Variable v) <- variable
                 return $ AST.FieldAccess e v

dereference :: Parser (AST.LValue AST.PositionedExpression)
dereference = (do Lexer.reservedOp "*"
                  AST.Dereference <$> expr) <?> "dereference"

fieldAccessOp = Infix ((do pos <- getPosition
                           Lexer.reservedOp "."
                           return $ result pos) <?> "field access")
                      AssocLeft
  -- This can lead to a hard crash.  Shh...
  where result pos struct (AST.EW (AST.LValueExpr (AST.Variable v)) _)
          = AST.EW (AST.LValueExpr (AST.FieldAccess struct v)) pos

binary op = Infix (do pos <- getPosition
                      Lexer.reservedOp op
                      let f x y = AST.EW (AST.Binop x op y) pos
                      return f) AssocLeft

table = [fieldAccessOp] : (fmap binary <$>
        [ ["*", "/"]
        , ["+", "-"]
        , ["<", "<=", ">", ">="]
        , ["==", "!="]
        , ["&&"], ["||"] ])

-- Parsing types.

typeParser :: Parser (Annotated AST.Type)
typeParser = do pos <- getPosition
                n <- Lexer.tname
                let name = Annotated (AST.NamedType n) pos
                ptrs <- map AST.pos <$> many (annotate $ Lexer.reservedOp "*")
                let aux pos t = Annotated (AST.Pointer t) pos
                return $ foldr aux name ptrs

-- Parsing statements.
statement :: Parser AST.PositionedStatement
statement = compoundDeclaration
        <|> assignment
        <|> AST.ExpressionStatement <$> expr
        <|> returnStatement
        -- <|> ifStatement
        <?> "statement"

returnStatement :: Parser AST.PositionedStatement
returnStatement = (do r <- Lexer.reserved "return"
                      (AST.Return <$> expr) <|> return AST.VoidReturn)
              <?> "return statement"

assignment :: Parser AST.PositionedStatement
assignment = do p <- getPosition
                l <- lvalue
                ((do Lexer.equals
                     r <- expr
                     return $ AST.Assignment l r) <?> "assignment")
                   <|> let e = AST.EW (AST.LValueExpr l) p
                        in return $ AST.ExpressionStatement e

declaration :: Parser AST.ValueDeclaration
declaration = do t <- typeParser
                 AST.Variable v <- variable
                 return $ AST.ValueDeclaration (AST.contents t) v

compoundDeclaration :: Parser AST.PositionedStatement
compoundDeclaration = do d <- annotate declaration
                         (do Lexer.equals
                             e <- expr
                             return $ AST.CompoundDeclaration d e)
                           <|> return (AST.Declaration $ AST.contents d)

semiFollowed :: Parser a -> Parser [a]
semiFollowed p = many $ do s <- p
                           Lexer.semi 
                           return s

block :: Parser (AST.Block AST.PositionedExpression)
block = semiFollowed $ annotate statement

ifStatement :: Parser AST.PositionedStatement
ifStatement = do Lexer.reserved "if"
                 cond <- expr
                 ifBlock <- Lexer.braces block
                 elseBlock <- (do try $ Lexer.reserved "else"
                                  Lexer.braces block) <|> return []
                 return $ AST.IfStatement cond ifBlock elseBlock

--
-- Top-level parsing.
--

topLevel :: Parser AST.PositionedTopLevel
topLevel = structDeclaration <|> functionDefinition <?> "toplevel form"

structDeclaration :: Parser AST.PositionedTopLevel
structDeclaration = (do Lexer.reserved "struct"
                        name <- Lexer.tname
                        (do fields <- Lexer.braces members
                            return $ AST.StructDeclaration name fields)
                           <|> return (AST.TypeDeclaration name))
                      <?> "struct definition"
  where members = semiFollowed $ annotate declaration

functionSignature :: Parser AST.FunctionSignature
functionSignature = (do Lexer.reserved "fn"
                        name <- Lexer.identifier
                        args <- argList
                        let void = Annotated AST.Void <$> getPosition
                        ret <- (Lexer.arrow >> typeParser) <|> void
                        return $ AST.FunctionSignature name args ret)
    <?> "function signature"
  where argList = Lexer.parens $ Lexer.commaSep (annotate declaration)

functionDefinition :: Parser AST.PositionedTopLevel
functionDefinition = annotate functionSignature >>= fnDeclOrDef

fnDeclOrDef :: Annotated AST.FunctionSignature
            -> Parser AST.PositionedTopLevel
fnDeclOrDef sig = (AST.FunctionDefinition sig <$> Lexer.braces block
                    <?> "function definition")
                <|> return (AST.FunctionDecl $ AST.contents sig)
