module TokenParser(
program,objCall,genericExpression,objCallNoEndF
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

-- A program is a sequence of statements
program :: Parser Stmt
program = do
            stmts <- many1 statement
            return $ Seq stmts

maybeProgram :: Parser Stmt
maybeProgram = do
            stmts <- many statement
            return $ Seq stmts

-- Possible statements (If, While, Assign, Print)
statement :: Parser Stmt
statement =  (ifStmt
           <|> printStmt
           <|> whileStmt
           <|> try callFunctionStmt
           <|> try changeStmt
           <|> try objCallStmt
           <|> assignLet
           <|> assignVar
           <|> returnStmt
           <|> parens statement) >>= (\stmt -> do
                                          optional semi
                                          return stmt
                              )


-- Arithmetic Expresion
aComplexExpression :: Parser AExpr
aComplexExpression = do
                     expr <- aExpression
                     case expr of VarA i -> fail "not Complex"
                                  AFCall _ ->  fail "not Complex"
                                  AOCall _ -> fail "not Complex"
                                  a -> return a

aTerm =
     try (fCallExpression >>= (AFCall >>> return))
     <|> try (liftM AOCall objCallResult)
     <|> try (identifier  >>= (VarA >>> return))
     <|> try (number  >>= (NumericConst >>> return))
     <|> parens aExpression

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

-- Boolean Expresion
bComplexExpression :: Parser BExpr
bComplexExpression = do
                     expr <- bExpression
                     case expr of VarB i -> fail "not Complex"
                                  BFCall _ -> fail "not Complex"
                                  BOCall _ -> fail "not Complex"
                                  b -> return b

bTerm =
         try compareExpresion
         <|> try (liftM BFCall fCallExpression)
         <|> try (liftM BOCall objCallResult)
         <|> try (liftM VarB identifier)
         <|> try (reserved "true"  >> return (BConst True) )
         <|> try (reserved "false" >> return (BConst False))
         <|> parens bExpression

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
         <|> (reservedOp "<=" >> return LessE)
         <|> (reservedOp ">=" >> return GreaterE)
         <|> (reservedOp "==" >> return Equal)
         <|> (reservedOp "!=" >> return NEqual)

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
        stmts <- braces maybeProgram
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
                    try (aComplexExpression  >>= (AlgebraicE >>> return))
                    <|> try (bComplexExpression >>= (BooleanE >>> return))
                    <|> try (objCallResult >>= (ObjCallE >>> return))
                    <|> try (fCallExpression >>= (FunctionCallE >>> return))
                    <|> try (sComplexExpression  >>= (StringE >>> return))
                    <|> try (identifier >>= (IdentifierE >>> return))
-- If Statement
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- parens bExpression
     stmt1 <- braces maybeProgram
     stmt2 <- option Skip (do {
                                reserved "else"
                                ;s2 <- braces maybeProgram
                                ; return s2
                                })
     return $ If cond stmt1 stmt2

-- While Statement
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- parens bExpression
     stmt <- braces maybeProgram
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
                    readNumStmt
                    <|> readLnStmt
                    <|> try assignB
                    <|> try assignA
                    <|> try assignStr
                    <|> try assignObjD
                    <|> try assignObjC
                    <|> try assignFD
                    <|> try assignFC
                    <|> assignI

makeAssignable::Parser a -> (a -> AssignableE) -> Parser AssignableE
makeAssignable parser mapper = parser >>= (mapper >>> return)

assignA     = makeAssignable aComplexExpression      (ValueE . AlgebraicE)
assignB     = makeAssignable bComplexExpression      (ValueE . BooleanE)
assignI     = makeAssignable identifier       (ValueE . IdentifierE)
assignFC    = makeAssignable fCallExpression  (ValueE . FunctionCallE)
assignStr   = makeAssignable sComplexExpression (ValueE . StringE)
assignFD    = makeAssignable fDeclExpression  (FDeclare)
assignObjD  = makeAssignable objDec           (ODec)
assignObjC  = makeAssignable objCallResult    (ValueE . ObjCallE)


---- Object Call

data ObjectParser = ObjectParser String ObjectParser
                  | ObjectFParser FCExpr ObjectParser
                  | ObjectBrParser String GenericExpr ObjectParser
                  | ObjectFEnd FCExpr
                  | ObjectIEnd String
                  | ObjectBrEnd String GenericExpr deriving(Show)

objCallStmt::Parser Stmt
objCallStmt = objCallResult >>= (OCall >>> return)

objCallResult::Parser ObjCall
objCallResult = objCall >>= (parserToObj >>> return)

parserToObj::ObjectParser -> ObjCall
parserToObj (ObjectParser  s (ObjectFEnd f))    = ObjFCall (ObjIBase s) f
parserToObj (ObjectParser  s (ObjectIEnd i))    = ObjCall  (ObjIBase s) i
parserToObj (ObjectParser  s (ObjectBrEnd br g)) = ObjBrCall  (ObjIBase s) br g
parserToObj (ObjectParser  s o)                 = preappendObjI (parserToObj o) s

parserToObj (ObjectFParser fc (ObjectFEnd f))     = ObjFCall (ObjFBase fc) f
parserToObj (ObjectFParser fc (ObjectIEnd i))     = ObjCall  (ObjFBase fc) i
parserToObj (ObjectFParser fc (ObjectBrEnd br g))  = ObjBrCall  (ObjFBase fc) br g
parserToObj (ObjectFParser fc o)                   = preappendObjF (parserToObj o) fc

parserToObj (ObjectBrParser s p (ObjectFEnd f))      = ObjFCall (ObjBrBase s p) f
parserToObj (ObjectBrParser s p (ObjectIEnd i))      = ObjCall  (ObjBrBase s p) i
parserToObj (ObjectBrParser s p (ObjectBrEnd br g))  = ObjBrCall  (ObjBrBase s p) br g
parserToObj (ObjectBrParser s p o)                  = preappendObjBr (parserToObj o) s p

parserToObj (ObjectBrEnd br p)                        = ObjBrBase br p


preappendObjI::ObjCall -> String -> ObjCall
preappendObjI (ObjFBase fcall)   s  = ObjFCall (ObjIBase s) fcall
preappendObjI (ObjIBase ident)   s  = ObjCall  (ObjIBase s) ident
preappendObjI (ObjBrBase br p)   s  = ObjBrCall (ObjIBase s) br p
preappendObjI (ObjCall o ident ) s  = ObjCall  (preappendObjI o s) ident
preappendObjI (ObjFCall o fcall) s  = ObjFCall (preappendObjI o s) fcall
preappendObjI (ObjBrCall o br p) s  = ObjBrCall (preappendObjI o s) br p

preappendObjF::ObjCall -> FCExpr -> ObjCall
preappendObjF (ObjFBase fcall)   f  = ObjFCall (ObjFBase f) fcall
preappendObjF (ObjIBase ident)   f  = ObjCall  (ObjFBase f) ident
preappendObjF (ObjBrBase br p)   f  = ObjBrCall  (ObjFBase f) br p
preappendObjF (ObjCall o ident)  f  = ObjCall  (preappendObjF o f) ident
preappendObjF (ObjFCall o fcall) f  = ObjFCall (preappendObjF o f) fcall
preappendObjF (ObjBrCall o br p) f  = ObjBrCall (preappendObjF o f) br p

preappendObjBr::ObjCall -> String -> GenericExpr -> ObjCall
preappendObjBr (ObjFBase fcall)   br p  = ObjFCall (ObjBrBase br p) fcall
preappendObjBr (ObjIBase ident)   br p  = ObjCall  (ObjBrBase br p) ident
preappendObjBr (ObjBrBase s i)   br p  = ObjBrCall  (ObjBrBase br p) s i
preappendObjBr (ObjCall o ident)  br p  = ObjCall  (preappendObjBr o br p) ident
preappendObjBr (ObjFCall o fcall) br p  = ObjFCall (preappendObjBr o br p) fcall
preappendObjBr (ObjBrCall o s i) br p  = ObjBrCall (preappendObjBr o br p) s i

objCall::Parser ObjectParser
objCall =  try (fToObjectCall objCallAndBase)
            <|> try (brToObjectCall objCallAndBase)
            <|> try (baseBrCall)
            <|> (iToObjectCall objCallAndBase)

objCallAndBase = try (fToObjectCall objCallAndBase)
                 <|> try (brToObjectCall objCallAndBase)
                 <|> try (iToObjectCall objCallAndBase)
                 <|> try (noSpacesFCall >>= (return . ObjectFEnd))
                 <|> try (noSpacesBrCall)
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

baseBrCall:: Parser ObjectParser
baseBrCall =
             do
             name <- noSpacesIdentifier
             parameter <- brackets genericExpression
             return $ ObjectBrEnd name parameter

noSpacesBrCall:: Parser ObjectParser
noSpacesBrCall =
              do
              name <- noSpacesIdentifier
              parameter <- noSpacesBrackets genericExpression
              return $ ObjectBrEnd name parameter

brToObjectCall::Parser ObjectParser -> Parser ObjectParser
brToObjectCall callAndBase =
              do
              name <- noSpacesIdentifier
              g <- noSpacesBrackets genericExpression
              noSpacesReservedOp "."
              o <- objCallAndBase
              whiteSpace
              return $ ObjectBrParser name g o

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
            <|> try (brToObjectCall objCallAndBaseNoEndF)
            <|> try (baseBrCall)
            <|> (iToObjectCall objCallAndBaseNoEndF)

objCallAndBaseNoEndF = try (fToObjectCall objCallAndBaseNoEndF)
                 <|> try (brToObjectCall objCallAndBaseNoEndF)
                 <|> try (iToObjectCall objCallAndBaseNoEndF)
                 <|> try (noSpacesBrCall)
                 <|> (noSpacesIdentifier >>= (return . ObjectIEnd))
---- Object Def
objDec::Parser ObjDec
objDec = do braces (commaSep objAssigns) >>= (return . ObjDec)
         where objAssigns =  do
                            name <- noSpacesIdentifier
                            reservedOp ":"
                            asign <- assignableParser
                            return $ (name, asign)

--- String Parser

printStmt :: Parser Stmt
printStmt = do
    reserved "print"
    content <- parens assignableParser
    return $ Print content

readNumStmt :: Parser AssignableE
readNumStmt = do
    reserved "readNum()"
    return $ ReadNum

readLnStmt :: Parser AssignableE
readLnStmt = do
    reserved "readLn()"
    return $ ReadLn

sExpression :: Parser SExpr
sExpression = buildExpressionParser sOperators sTerm

sComplexExpression = do
                     expr <- sExpression
                     case expr of StrGBase _ -> fail "not Complex"
                                  a -> return a

sTerm = try (noStrGenExpr >>= (return . StrGBase))
        <|> try (quotedString >>= (return . StrBase))
        <|> parens sTerm

noStrGenExpr:: Parser GenericExpr
noStrGenExpr = try (aComplexExpression  >>= (AlgebraicE >>> return))
               <|> try (bComplexExpression >>= (BooleanE >>> return))
               <|> try (objCallResult >>= (ObjCallE >>> return))
               <|> try (fCallExpression >>= (FunctionCallE >>> return))
               <|> try (identifier >>= (IdentifierE >>> return))

quotedString::Parser String
quotedString = do
  string <- between (char '"') (char '"') (many quotedStringChar)
  whiteSpace
  return string
  where
    quotedStringChar = escapedChar <|> normalChar
    escapedChar = (char '\\') *> (oneOf ['\\', '"'])
    normalChar = noneOf "\""


