module TokenParser(
program,objCall
) where

import Control.Monad
import Control.Arrow

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import LanguageDef
import NoSpacesParsec
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
            stmts <- many1 statement
            return $ Seq stmts

-- Possible statements (If, While, Assign, Print)
statement :: Parser Stmt
statement =  (ifStmt
           <|> whileStmt
           <|> try callFunctionStmt
           <|> try changeStmt
           <|> try objCallStmt
           <|> assignLet
           <|> assignVar
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

assignStmtGeneric :: String -> (String -> AssignableE -> Stmt) -> Parser Stmt
assignStmtGeneric word mapper =  do
                            reserved word
                            var  <- identifier
                            reservedOp "="
                            expr <- assignableParser
                            return $ mapper var expr

returnStmt ::Parser Stmt
returnStmt = do
          reserved "return"
          expr <- assignableParser
          return $ Return expr

assignLet = assignStmtGeneric "let" AssignLet
assignVar = assignStmtGeneric "var" AssignVar

changeStmt ::Parser Stmt
changeStmt =  do
                var  <- valueHolderParser
                reservedOp "="
                expr <- assignableParser
                return $ ChangeVal var expr

valueHolderParser::Parser ValueHolder
valueHolderParser = try (objCallNoEndF >>= return . parserToObj >>= (return . ObjectVH))
                    <|> (identifier >>= (return . IdentVH))


assignableParser:: Parser AssignableE
assignableParser = do
                    try assignObjD
                    <|> try assignObjC
                    <|> try assignA
                    <|> try assignB
                    <|> try assignFC
                    <|> try assignFD
                    <|> assignI

makeAssignable::Parser a -> (a -> AssignableE) -> Parser AssignableE
makeAssignable parser mapper = do
                               expr <- parser
                               return $ mapper expr

assignA  = makeAssignable aExpression     (ValueE . AlgebraicE)
assignB  = makeAssignable bExpression     (ValueE . BooleanE)
assignI  = makeAssignable identifier      (ValueE . IdentifierE)
assignFC = makeAssignable fCallExpression (ValueE . FunctionCallE)
assignFD = makeAssignable fDeclExpression (FDeclare)
assignObjD = makeAssignable objDec     (ODec)
assignObjC = makeAssignable objCall     (parserToObj >>> (ValueE . ObjCallE))


---- Object Call

data ObjectParser = ObjectParser String ObjectParser | ObjectFParser FCExpr ObjectParser | ObjectFEnd FCExpr | ObjectIEnd String

objCallStmt::Parser Stmt
objCallStmt = do
                call <- objCall
                return $ OCall (parserToObj call)

parserToObj::ObjectParser -> ObjCall
parserToObj (ObjectParser  s (ObjectFEnd f))    = ObjFCall (ObjIBase s) f
parserToObj (ObjectParser  s (ObjectIEnd i))    = ObjCall  (ObjIBase s) i
parserToObj (ObjectParser  s o)                 = preappendObjI (parserToObj o) s
parserToObj (ObjectFParser fc (ObjectFEnd f))    = ObjFCall (ObjFBase fc) f
parserToObj (ObjectFParser fc (ObjectIEnd i))     = ObjCall  (ObjFBase fc) i
parserToObj (ObjectFParser fc o)                  = preappendObjF (parserToObj o) fc

preappendObjI::ObjCall -> String -> ObjCall
preappendObjI (ObjFBase fcall)   s  = ObjFCall (ObjIBase s) fcall
preappendObjI (ObjIBase ident)   s  = ObjCall  (ObjIBase s) ident
preappendObjI (ObjCall o ident ) s  = ObjCall  (preappendObjI o s) ident
preappendObjI (ObjFCall o fcall) s  = ObjFCall (preappendObjI o s) fcall

preappendObjF::ObjCall -> FCExpr -> ObjCall
preappendObjF (ObjFBase fcall)   f  = ObjFCall (ObjFBase f) fcall
preappendObjF (ObjIBase ident)   f  = ObjCall  (ObjFBase f) ident
preappendObjF (ObjCall o ident)  f  = ObjCall  (preappendObjF o f) ident
preappendObjF (ObjFCall o fcall) f  = ObjFCall (preappendObjF o f) fcall


objCall::Parser ObjectParser
objCall =  try (fToObjectCall objCallAndBase)
            <|> (iToObjectCall objCallAndBase)

objCallAndBase = try (fToObjectCall objCallAndBase)
                 <|> try (iToObjectCall objCallAndBase)
                 <|> try (noSpacesFCall >>= (return . ObjectFEnd))
                 <|> (noSpacesIdentifier >>= (return . ObjectIEnd))

noSpacesFCall:: Parser FCExpr
noSpacesFCall =
              do
              name <- noSpacesIdentifier
              parameters <- noSpacesParens parameterGenericExpr
              return $ FCExpr name parameters

fToObjectCall::Parser ObjectParser -> Parser ObjectParser
fToObjectCall callAndBase =
              do
              f <- noSpacesFCall
              noSpacesReservedOp "."
              o <- objCallAndBase
              whiteSpace
              return $ ObjectFParser f o

iToObjectCall::Parser ObjectParser -> Parser ObjectParser
iToObjectCall callAndBase =
              do
              i <- noSpacesIdentifier
              noSpacesReservedOp "."
              o <- callAndBase
              whiteSpace
              return $ ObjectParser i o



objCallNoEndF::Parser ObjectParser
objCallNoEndF =  try (fToObjectCall objCallAndBaseNoEndF)
            <|> (iToObjectCall objCallAndBaseNoEndF)


objCallAndBaseNoEndF = try (fToObjectCall objCallAndBaseNoEndF)
                 <|> try (iToObjectCall objCallAndBaseNoEndF)
                 <|> (noSpacesIdentifier >>= (return . ObjectIEnd))

---- Object Def
objDec::Parser ObjDec
objDec = do braces (commaSep objAssigns) >>= (return . ObjDec)
         where objAssigns =  do
                            name <- noSpacesIdentifier
                            reservedOp ":"
                            asign <- assignableParser
                            return $ (name, asign)

