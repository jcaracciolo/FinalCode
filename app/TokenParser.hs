module TokenParser(
program
) where

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import LanguageDef
import DataTypes


quotedString = do
  string <- between (char '"') (char '"') (many quotedStringChar)
  return string
  where
    quotedStringChar = escapedChar <|> normalChar
    escapedChar = (char '\\') *> (oneOf ['\\', '"'])
    normalChar = noneOf "\""


-- A program is a sequence of statements
program :: Parser Stmt
program = do
            stmts <- many statement
            return $ Seq stmts

-- Possible statements (If, While, Assign, Print)
statement :: Parser Stmt
statement =  ifStmt
           <|> whileStmt
           <|> try assignStmtA
           <|> assignStmtB
           <|> try changeStmtA
           <|> changeStmtB
           <|> printStmt

-- Arithmetic Expresion
aTerm =  parens aExpression
     <|> liftM IntConst integer
     <|> liftM VarA identifier

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

-- Boolean Expresion
bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BConst True) )
     <|> (reserved "false" >> return (BConst False))
     <|> try compareExpresion
     <|> liftM VarB identifier

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
         <|> (reservedOp "<=" >> return LessE)
         <|> (reservedOp ">=" >> return GreaterE)
         <|> (reservedOp "==" >> return Equal)

compareExpresion =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ BCompare op a1 a2

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm


-- Print Statement
printStmt :: Parser Stmt
printStmt = do
    reserved "print"
    content <- parens quotedString
    return $ Print content


-- If Statement
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- parens bExpression
     stmt1 <- braces program
     stmt2 <- option Skip (do {
                                reserved "else"
                                ;s2 <- braces program
                                ; return s2
                                })
     return $ If cond stmt1 stmt2

-- While Statement
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- parens bExpression
     stmt <- braces program
     return $ While cond stmt


-- Assign Statement

assignStmtGeneric :: Parser a -> (String -> a -> Stmt) -> Parser Stmt
assignStmtGeneric parser mapper =
                                  do
                                     reserved "let"
                                     var  <- identifier
                                     reservedOp "="
                                     expr <- parser
                                     return $ mapper var expr

assignStmtA = assignStmtGeneric aExpression AssignA
assignStmtB = assignStmtGeneric bExpression AssignB

-- change statement
changeStmtGeneric :: Parser a -> (String -> a -> Stmt) -> Parser Stmt
changeStmtGeneric parser mapper =
  do var  <- identifier
     reservedOp "="
     expr <- parser
     return $ mapper var expr

changeStmtA = changeStmtGeneric aExpression ChangeValA
changeStmtB = changeStmtGeneric bExpression ChangeValB


