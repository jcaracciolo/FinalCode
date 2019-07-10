module TokenParser(
program
) where

import Control.Monad
import Control.Arrow

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
statement =  (ifStmt
           <|> whileStmt
           <|> try callFunctionStmt
           <|> assignLet
           <|> assignVar
           <|> changeVar
           <|> returnStmt
           <|> printStmt
           <|> parens statement) >>= (\stmt -> do
                                          optional semi
                                          return stmt
                              )


-- Arithmetic Expresion
aTerm =  liftM IntConst integer
     <|> try (liftM AFCall fCallExpression)
     <|> liftM VarA identifier
     <|> parens aExpression

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

-- Boolean Expresion
bTerm = (reserved "true"  >> return (BConst True) )
     <|> (reserved "false" >> return (BConst False))
     <|> try (liftM BFCall fCallExpression)
     <|> try compareExpresion
     <|> try (liftM VarB identifier)
     <|> parens bTerm

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

-- Function Expression
fDeclExpression:: Parser FDExpr
fDeclExpression = do
        noSpacesReserved "function"
        notFollowedBy whiteSpace
        parameters <- parens parameterExpression
        stmts <- braces program
        return $ FDExpr parameters stmts

parameterExpression:: Parser [String]
parameterExpression = commaSep identifier

callFunctionStmt:: Parser Stmt
callFunctionStmt = (fCallExpression >>= (FCall >>> return))

fCallExpression:: Parser FCExpr
fCallExpression = do
                    name <- noSpacesIdentifier
                    parameters <- parens parameterGenericExpr
                    return $ FCExpr name parameters

parameterGenericExpr::Parser [GenericExpr]
parameterGenericExpr = commaSep genericExpression

genericExpression:: Parser GenericExpr
genericExpression =
                    try (fCallExpression >>= (FunctionCallE >>> return))
                    <|> try (identifier >>= (IdentifierE >>> return))
                    <|> (aExpression  >>= (AlgebraicE >>> return))
                    <|> (bExpression >>= (BooleanE >>> return))

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

assignStmtGeneric :: Maybe String -> Parser a -> (String -> a -> Stmt) -> Parser Stmt
assignStmtGeneric word parser mapper = case word of Nothing -> assignment
                                                    Just w ->  reserved w >> assignment
                                            where assignment = do
                                                                var  <- identifier
                                                                reservedOp "="
                                                                expr <- parser
                                                                return $ mapper var expr

returnStmtGeneric :: Parser a -> (a -> AssignableE) -> Parser Stmt
returnStmtGeneric parser mapper = do
                                    reserved "return"
                                    expr <- parser
                                    return $ Return (mapper expr)

assignLet = try assignLetA
        <|> try assignLetB
        <|> try assignLetFC
        <|> try assignLetFD
        <|> try assignLetI

assignVar = try assignVarA
        <|> try assignVarB
        <|> try assignVarFC
        <|> try assignVarFD
        <|> try assignVarI

changeVar = try changeFC
        <|> try changeA
        <|> try changeB
        <|> try changeFD
        <|> try changeI

returnStmt = returnStmtA
         <|> returnStmtB
         <|> returnStmtFC
         <|> returnStmtFD
         <|> returnStmtI


assignLetA  = assignStmtGeneric (Just "let") aExpression     (flip (flip AssignLet . ValueE . AlgebraicE))
assignLetB  = assignStmtGeneric (Just "let") bExpression     (flip (flip AssignLet . ValueE . BooleanE))
assignLetI  = assignStmtGeneric (Just "let") identifier      (flip (flip AssignLet . ValueE . IdentifierE))
assignLetFC = assignStmtGeneric (Just "let") fCallExpression (flip (flip AssignLet . ValueE . FunctionCallE))
assignLetFD = assignStmtGeneric (Just "let") fDeclExpression (flip (flip AssignLet . FDeclare))

assignVarA  = assignStmtGeneric (Just "var") aExpression     (flip (flip AssignVar . ValueE . AlgebraicE))
assignVarB  = assignStmtGeneric (Just "var") bExpression     (flip (flip AssignVar . ValueE . BooleanE))
assignVarI  = assignStmtGeneric (Just "var") identifier      (flip (flip AssignVar . ValueE . IdentifierE))
assignVarFC = assignStmtGeneric (Just "var") fCallExpression (flip (flip AssignVar . ValueE . FunctionCallE))
assignVarFD = assignStmtGeneric (Just "var") fDeclExpression (flip (flip AssignVar . FDeclare))

changeA  = assignStmtGeneric Nothing aExpression     (flip (flip ChangeVal . ValueE . AlgebraicE))
changeB  = assignStmtGeneric Nothing bExpression     (flip (flip ChangeVal . ValueE . BooleanE))
changeI  = assignStmtGeneric Nothing identifier      (flip (flip ChangeVal . ValueE . IdentifierE))
changeFC = assignStmtGeneric Nothing fCallExpression (flip (flip ChangeVal . ValueE . FunctionCallE))
changeFD = assignStmtGeneric Nothing fDeclExpression (flip (flip ChangeVal . FDeclare))

returnStmtA  = returnStmtGeneric aExpression     (ValueE . AlgebraicE)
returnStmtB  = returnStmtGeneric bExpression     (ValueE . BooleanE)
returnStmtI  = returnStmtGeneric identifier      (ValueE . IdentifierE)
returnStmtFC = returnStmtGeneric fCallExpression (ValueE . FunctionCallE)
returnStmtFD = returnStmtGeneric fDeclExpression (FDeclare)



