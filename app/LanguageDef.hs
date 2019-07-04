module LanguageDef(
languageDef,
lexer,
identifier,
reserved,
reservedOp,
braces,
parens,
integer,
semi,
whiteSpace,
aOperators,
bOperators
) where

import DataTypes
import System.IO
import System.Environment
import Control.Monad
import Data.Tuple
import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "else"
                                     , "else if"
                                     , "while"
                                     , "print"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "let"
                                     , "var"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", "="
                                     , "<", ">", ">=", "<=", "==", "and", "or", "not"
                                     ]
           }


aOperators =[
                [ Prefix (reservedOp "-"   >> return Neg ) ],
                [
                   Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                   Infix  (reservedOp "/"   >> return (ABinary Divide)) AssocLeft
                ],
                [
                 Infix  (reservedOp "+"   >> return (ABinary Add)     ) AssocLeft,
                 Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft
                ]
            ]

bOperators = [
                [ Prefix (reservedOp "not" >> return Not) ],
                [
                 Infix  (reservedOp "and" >> return (BBinary And)) AssocLeft,
                 Infix  (reservedOp "or"  >> return (BBinary Or)) AssocLeft
                ]
             ]

lexer = Token.makeTokenParser languageDef


identifier  = Token.identifier  lexer -- parses an identifier
reserved    = Token.reserved    lexer -- parses a reserved name
reservedOp  = Token.reservedOp  lexer -- parses an operator
braces      = Token.braces      lexer -- parses braces
parens      = Token.parens      lexer -- parses surrounding parenthesis
integer     = Token.integer      lexer -- parses an integer
semi        = Token.semi         lexer -- parses a semicolon
whiteSpace  = Token.whiteSpace   lexer -- parses whiteSpace



