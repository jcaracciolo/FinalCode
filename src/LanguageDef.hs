module LanguageDef(
languageDef,
lexer,
identifier,
reserved,
reservedOp,
braces,
parens,
number,
semi,
whiteSpace,
aOperators,
bOperators,
commaSep,
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
                                     , "function"
                                     , "return"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", "="
                                     , "<", ">", ">=", "<=", "==", "&&", "||", "!", ".", ":"
                                     ]
           , Token.caseSensitive = True
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
                [ Prefix (reservedOp "!" >> return Not) ],
                [
                 Infix  (reservedOp "&&" >> return (BBinary And)) AssocLeft,
                 Infix  (reservedOp "||"  >> return (BBinary Or)) AssocLeft
                ]
             ]

lexer = Token.makeTokenParser languageDef
identifier' = Token.identifier  lexer
reserved    = Token.reserved    lexer -- parses a reserved name
reservedOp  = Token.reservedOp  lexer -- parses an operator
braces      = Token.braces      lexer -- parses braces
parens      = Token.parens      lexer -- parses surrounding parenthesis
integer     = Token.integer lexer -- parses an integer
float       = Token.float lexer -- parses an integer
number      = try float <|> (integer >>= (return . fromIntegral))
semi        = Token.semi        lexer -- parses a semicolon
commaSep    = Token.commaSep    lexer -- parses a semicolon
whiteSpace  = Token.whiteSpace  lexer -- parses whiteSpace
identifier  = (try (parens identifier)) <|> identifier'
