{-|
Module      : Craeft.Lexer
Description : A Parsec-based lexical analyzer for Craeft.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Craeft.Lexer where

import qualified Data.Char as Char
import           Control.Monad

import           Text.Parsec.String (Parser)
import           Text.Parsec.Char
import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    names = ["fn", "struct", "return", "if", "else"]
    operators = ["+", "/", "-", "+",
                 "&&", "||", "&", ".", "->", "<:", ":>",
                 "==", "!=", ">=", "<=", ">", "<",
                 "="]
    style = emptyDef {
               Tok.commentLine = "//"
             , Tok.commentStart = "/*"
             , Tok.commentEnd = "*/"
             , Tok.nestedComments = True
             , Tok.identStart = letter <|> char '_'
             , Tok.identLetter = alphaNum <|> char '_'
             , Tok.reservedOpNames = operators
             , Tok.reservedNames = names }

braces :: Parser a -> Parser a
braces = Tok.braces lexer

integer :: Parser Integer
integer = Tok.integer lexer

unsigned :: Parser Integer
unsigned = Tok.natural lexer

float :: Parser Double
float = Tok.float lexer

string :: Parser String
string = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser ()
semi = void $ Tok.semi lexer

identifier :: Parser String
identifier = try $ do (x:xs) <- Tok.identifier lexer <?> "identifier"
                      guard $ Char.isLower x
                      return $ x:xs

tname :: Parser String
tname = try $ do (x:xs) <- Tok.identifier lexer <?> "type name"
                 guard $ Char.isUpper x
                 return $ x:xs

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

openTemplate :: Parser ()
openTemplate = void $ reservedOp "<:"

closeTemplate :: Parser ()
closeTemplate = void $ reservedOp ":>"

arrow :: Parser ()
arrow = void $ reservedOp "->"

equals :: Parser ()
equals = void $ reservedOp "="

dot :: Parser ()
dot = void $ Tok.dot lexer

startLexer :: Parser ()
startLexer = Tok.whiteSpace lexer
