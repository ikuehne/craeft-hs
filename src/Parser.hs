module Parser where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Char
import qualified AST
import qualified Lexer

--
-- Parsing expressions.
--

type ExpressionParser = Parser (AST.Annotated AST.Expression)

annotate :: Parser a -> Parser (AST.Annotated a)
annotate p = flip AST.Annotated <$> getPosition <*> p

unsigned = AST.UIntLiteral <$> Lexer.unsigned
signed = AST.IntLiteral <$> Lexer.integer
float = AST.FloatLiteral <$> Lexer.float
string = AST.StringLiteral <$> Lexer.string

expr = buildExpressionParser table term <?> "expression"

term :: ExpressionParser
term = Lexer.parens expr
   <|> annotate (unsigned
             <|> signed
             <|> float
             <|> Parser.string
             <|> AST.LValueExpr <$> (dereference <|> variable))
   <?> "simple expression"

lvalue :: Parser AST.LValue
lvalue = dereference <|> variable <|> fieldAccess <?> "lvalue"

variable :: Parser AST.LValue
variable = AST.Variable <$> Lexer.identifier <?> "variable"

fieldAccess :: Parser AST.LValue
fieldAccess = do e <- expr
                 Lexer.dot
                 (AST.Variable v) <- variable
                 return $ AST.FieldAccess e v

dereference :: Parser AST.LValue
dereference = (do Lexer.reservedOp "*"
                  AST.Dereference <$> expr) <?> "dereference"

fieldAccessOp = Infix ((do pos <- getPosition
                           Lexer.reservedOp "."
                           return $ result pos) <?> "field access")
                      AssocLeft
  -- This can lead to a hard crash.  Shh...
  where result pos struct (AST.Annotated (AST.LValueExpr (AST.Variable v)) _)
          = AST.Annotated (AST.LValueExpr (AST.FieldAccess struct v)) pos

binary op = Infix (do pos <- getPosition
                      Lexer.reservedOp op
                      let f x y = AST.Annotated (AST.Binop x op y) pos
                      return f) AssocLeft

table = [fieldAccessOp] : (fmap binary <$>
        [ ["*", "/"]
        , ["+", "-"]
        , ["<", "<=", ">", ">="]
        , ["==", "!="]
        , ["&&"], ["||"] ])

-- Parsing types.

typeParser :: Parser (AST.Annotated AST.Type)
typeParser = do pos <- getPosition
                n <- Lexer.tname
                let name = AST.Annotated (AST.NamedType n) pos
                ptrs <- map AST.pos <$> many (annotate $ Lexer.reservedOp "*")
                let aux pos t = AST.Annotated (AST.Pointer t) pos
                return $ foldr aux name ptrs

-- Parsing statements.
statement :: Parser AST.Statement
statement = compoundDeclaration
        <|> assignment
        <|> AST.ExpressionStatement <$> expr
        <|> returnStatement
        -- <|> ifStatement
        <?> "statement"

returnStatement :: Parser AST.Statement
returnStatement = (do r <- Lexer.reserved "return"
                      (AST.Return <$> expr) <|> return AST.VoidReturn)
              <?> "return statement"

assignment :: Parser AST.Statement
assignment = do l <- annotate lvalue
                ((do Lexer.equals
                     r <- expr
                     return $ AST.Assignment l r) <?> "assignment")
                   <|> let c = AST.contents l
                           e = AST.Annotated (AST.LValueExpr c) (AST.pos l)
                        in return $ AST.ExpressionStatement e

declaration :: Parser AST.ValueDeclaration
declaration = do t <- typeParser
                 AST.Variable v <- variable
                 return $ AST.ValueDeclaration (AST.contents t) v

compoundDeclaration :: Parser AST.Statement
compoundDeclaration = do d <- annotate declaration
                         (do Lexer.equals
                             e <- expr
                             return $ AST.CompoundDeclaration d e)
                           <|> return (AST.Declaration $ AST.contents d)

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
functionSignature = (do Lexer.reserved "fn"
                        name <- Lexer.identifier
                        args <- argList
                        let void = AST.Annotated AST.Void <$> getPosition
                        ret <- (Lexer.arrow >> typeParser) <|> void
                        return $ AST.FunctionSignature name args ret)
    <?> "function signature"
  where argList = Lexer.parens $ Lexer.commaSep (annotate declaration)

functionDefinition :: Parser AST.TopLevel
functionDefinition = annotate functionSignature >>= fnDeclOrDef

fnDeclOrDef :: AST.Annotated AST.FunctionSignature -> Parser AST.TopLevel
fnDeclOrDef sig = (AST.FunctionDefinition sig <$> Lexer.braces block
                    <?> "function definition")
                <|> return (AST.FunctionDecl $ AST.contents sig)
