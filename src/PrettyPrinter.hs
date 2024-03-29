module PrettyPrinter(
mainPrettyPrint,
prettyPrintFD,
prettyPrintOD,
toStringOut
)where

import DataTypes
import TokenParser
import System.Environment
import Control.Monad.State
import Control.Arrow
import Data.List
import LanguageDef
import qualified Text.ParserCombinators.Parsec as Parsec

type PrinterState = (TabCount, String)
type TabCount = Int


addScope::PrinterState -> PrinterState
addScope (t, s) = (t+1, s)

removeScope::PrinterState -> PrinterState
removeScope (t, s) = (t-1, s)

appendState:: String -> PrinterState -> PrinterState
appendState s2 (t, s) = (t, s ++ s2)

mAppend::String -> State PrinterState ()
mAppend s = modify(appendState s)

generateTabString::State PrinterState String
generateTabString = do state <- get
                       return $ replicate (fst state) '\t'

newLine:: State PrinterState ()
newLine   = do
            tabString <- generateTabString
            modify(appendState $ "\n" ++ tabString)


type Precedence = Int
type PrettyReturn = (String, Precedence)
wrapPrecedenceToLeft::Precedence -> String -> PrettyReturn -> PrettyReturn -> String
wrapPrecedenceToLeft p op (s1,p1) (s2,p2) = (if p1 > p then "(" ++ s1 ++ ")" else s1)
                                            ++ op ++
                                            (if p2 >= p then "(" ++ s2 ++ ")" else s2)

prettyPrintABin::AExpr -> AExpr -> String -> Precedence -> State PrinterState PrettyReturn
prettyPrintABin aexpr1 aexpr2 op p = do
                             i1 <- prettyPrintA aexpr1
                             i2 <- prettyPrintA aexpr2
                             return $ (wrapPrecedenceToLeft p op i1 i2, p)



wrap::Precedence -> String -> PrettyReturn -> PrettyReturn
wrap p op (s,i) = if i>=p then (op ++ "(" ++ s ++ ")", -2) else (op ++ s, p)

dropP::PrettyReturn -> State PrinterState String
dropP (s,i) = return s

-------   Algebraic Pretty Print



prettyPrintA::AExpr -> State PrinterState PrettyReturn
prettyPrintA (NumericConst i)               = return $ (show i, -1)
prettyPrintA (Neg aexpr)                    = prettyPrintA aexpr >>= \s -> return $ wrap 0 "-" s
prettyPrintA (ABinary Add expr1 expr2)      = prettyPrintABin expr1 expr2 " + " 2
prettyPrintA (ABinary Subtract expr1 expr2) = prettyPrintABin expr1 expr2 " - " 2
prettyPrintA (ABinary Multiply expr1 expr2) = prettyPrintABin expr1 expr2 " * " 1
prettyPrintA (ABinary Divide expr1 expr2)   = prettyPrintABin expr1 expr2 " / " 1
prettyPrintA (VarA s)                       = return (s, -1)
prettyPrintA (AFCall fcexpr)                = prettyPrintFC fcexpr >>= \s -> return (s, -1)
prettyPrintA (AOCall objC)                  = prettyPrintOC objC >>= \s -> return (s, -1)

-------- Boolean Pretty Print

prettyPrintBBin::BExpr -> BExpr -> String -> Precedence -> State PrinterState PrettyReturn
prettyPrintBBin bexpr1 bexpr2 op p = do
                             i1 <- prettyPrintB bexpr1
                             i2 <- prettyPrintB bexpr2
                             return $ (wrapPrecedenceToLeft p op i1 i2, p)

prettyPrintB:: BExpr -> State PrinterState PrettyReturn
-- prettyPrintB a = return $ (show a, 0)
prettyPrintB (BConst False)                       = return $ ("false", -1)
prettyPrintB (BConst True)                        = return $ ("true", -1)
prettyPrintB (Not bexpr)                          = prettyPrintB bexpr >>= \s -> return $ wrap 0 "!" s
prettyPrintB (BBinary And bexpr1 bexpr2)          = prettyPrintBBin bexpr1 bexpr2 " && " 4
prettyPrintB (BBinary Or bexpr1 bexpr2)           = prettyPrintBBin bexpr1 bexpr2 " || " 5
prettyPrintB (BCompare Greater aexpr1 aexpr2)     = prettyPrintABin aexpr1 aexpr2 " > " 3
prettyPrintB (BCompare GreaterE aexpr1 aexpr2)    = prettyPrintABin aexpr1 aexpr2 " >= " 3
prettyPrintB (BCompare Equal aexpr1 aexpr2)       = prettyPrintABin aexpr1 aexpr2 " == " 3
prettyPrintB (BCompare NEqual aexpr1 aexpr2)       = prettyPrintABin aexpr1 aexpr2 " != " 3
prettyPrintB (BCompare LessE aexpr1 aexpr2)       = prettyPrintABin aexpr1 aexpr2 " <= " 3
prettyPrintB (BCompare Less aexpr1 aexpr2)        = prettyPrintABin aexpr1 aexpr2 " < " 3
prettyPrintB (VarB s)                             = return (s, -1)
prettyPrintB (BFCall fcexpr)                      = prettyPrintFC fcexpr >>= \s -> return (s, -1)
prettyPrintB (BOCall objC)                        = prettyPrintOC objC >>= \s -> return (s, -1)

-------- Assignable Pretty Print
prettyPrintAssignable::AssignableE -> State PrinterState ()
prettyPrintAssignable (ValueE genericExpr)              = prettyPrintG genericExpr >>= mAppend
prettyPrintAssignable (FDeclare fdexpr)                 = prettyPrintFD fdexpr
prettyPrintAssignable (ODec odexpr)                     = prettyPrintOD odexpr
prettyPrintAssignable (ReadNum)                         = mAppend "readNum()"
prettyPrintAssignable (ReadLn)                          = mAppend "readLn()"

prettyPrintAssign::String -> String -> AssignableE -> State PrinterState ()
prettyPrintAssign keyword variable assign = do newLine
                                               mAppend keyword
                                               mAppend variable
                                               mAppend " = "
                                               prettyPrintAssignable assign



------ Function Pretty Print
prettyPrintFC::FCExpr -> State PrinterState String
prettyPrintFC (FCExpr fName exprs) = do
                                     params <- mapM prettyPrintG exprs
                                     return $ fName ++ "(" ++ (intercalate "," params) ++ ")"

prettyPrintFD::FDExpr -> State PrinterState ()
prettyPrintFD (FDExpr params stmts) = do mAppend "function("
                                         mAppend (intercalate "," params)
                                         mAppend ") {"
                                         modify addScope
                                         prettyPrint stmts
                                         modify removeScope
                                         newLine
                                         mAppend "}"
                                         return ()


prettyPrintG::GenericExpr -> State PrinterState String
prettyPrintG (AlgebraicE aexpr)                     = prettyPrintA aexpr >>= dropP
prettyPrintG (BooleanE bexpr)                       = prettyPrintB bexpr >>= dropP
prettyPrintG (IdentifierE name)                     = return name
prettyPrintG (FunctionCallE fcexpr)                 = prettyPrintFC fcexpr
prettyPrintG (ObjCallE oCall)                       = prettyPrintOC oCall
prettyPrintG (StringE sexpr)                        = prettyPrintS sexpr


prettyPrintS::SExpr -> State PrinterState String
prettyPrintS(StrBase s) = return $ show s
prettyPrintS(StrGBase g)  = prettyPrintG g
prettyPrintS(StrConcat sExpr1 sExpr2)  = do
                                         s1 <- prettyPrintS sExpr1
                                         s2 <- prettyPrintS sExpr2
                                         return $ s1 ++ " $ " ++ s2

-------- ObjectCall Pretty Print
prettyPrintOC::ObjCall -> State PrinterState String
prettyPrintOC (ObjCall o s)          = prettyPrintOC o >>= (\os -> return $ os ++ "." ++ s)
prettyPrintOC (ObjBrCall o br g)     = do
                                       os <- prettyPrintOC o
                                       p <- prettyPrintG g
                                       return $ os ++ "." ++ br ++ p ++ "[" ++ p ++ "]"
prettyPrintOC (ObjFCall o f)    = do os <- prettyPrintOC o
                                     fs <- prettyPrintFC f
                                     return $ os ++ "." ++ fs
prettyPrintOC (ObjFBase f)      = prettyPrintFC f
prettyPrintOC (ObjIBase s)      = return s
prettyPrintOC (ObjBrBase br g)      = do
                                      p <- prettyPrintG g
                                      return $ br ++ "[" ++ p ++ "]"


-------- ObjectDeclare Pretty Print
prettyPrintOD::ObjDec -> State PrinterState ()
prettyPrintOD (ObjDec vs)  = do mAppend "{"
                                modify addScope
                                prettyPrintODRec vs
                                modify removeScope
                                newLine
                                mAppend "}"
                                return ()

prettyPrintODRec::[(String, AssignableE)] -> State PrinterState ()
prettyPrintODRec []     = return ()
prettyPrintODRec (v:vs) = let (s, a) = v in
                          do newLine
                             mAppend s
                             mAppend ": "
                             prettyPrintAssignable a
                             mAppend ","
                             prettyPrintODRec vs
                             return ()

prettyPrintOT::[(String, VariableType)] -> State PrinterState ()
prettyPrintOT (vs)  = do mAppend "{"
                         modify addScope
                         prettyPrintOTVs vs
                         modify removeScope
                         newLine
                         mAppend "}"
                         return ()

prettyPrintOTVs::[(String, VariableType)] -> State PrinterState ()
prettyPrintOTVs []     = return ()
prettyPrintOTVs (v:vs) = let (s, a) = v in
                          do newLine
                             mAppend s
                             mAppend ": "
                             (t, _) <- get
                             mAppend (toStringWithState (t,"") a)
                             mAppend ","
                             prettyPrintOTVs vs
                             return ()
toStringOut::VariableType -> String
toStringOut (StrT s) = s
toStringOut a = toString a

toStringWithState::PrinterState -> VariableType -> String
toStringWithState s (FunctionT fdexpr)                = snd $ snd $ runState (prettyPrintFD fdexpr) s
toStringWithState s (ObjectT values)                  = snd $ snd $ runState (prettyPrintOT values) s
toStringWithState _ v = toString v

toString::VariableType -> String
toString (NumericT d)                               = show d
toString (StrT s)                                   = show s
toString (BoolT b)                                  = show b
toString (FunctionT fdexpr)                         = snd $ snd $ runState (prettyPrintFD fdexpr) (0, "")
toString (ObjectT values)                           = snd $ snd $ runState (prettyPrintOT values) (0, "")
toString (Undefined)                                = "undefined"

------- PrettyPrint Value Holder
prettyPrintVH::ValueHolder -> State PrinterState String
prettyPrintVH (IdentVH str) = return str
prettyPrintVH (ObjectVH oCall) = prettyPrintOC oCall


-------- Main Pretty Print

prettyPrint::Stmt -> State PrinterState ()
prettyPrint (Seq []) = return ()
prettyPrint (Seq (s:ss)) = prettyPrint (s) >> prettyPrint (Seq ss)
prettyPrint (AssignLet name assignable)   = prettyPrintAssign "let " name assignable
prettyPrint (AssignVar name assignable)   = prettyPrintAssign "var " name assignable
prettyPrint (ChangeVal vh assignable)     = prettyPrintVH vh >>= (\vhs -> prettyPrintAssign "" vhs assignable)
prettyPrint (Return assignable)           = do newLine
                                               mAppend "return "
                                               prettyPrintAssignable assignable

prettyPrint (If bexpr s1 s2)              = do newLine
                                               mAppend "if("
                                               cond <- (prettyPrintB bexpr >>= dropP)
                                               mAppend cond
                                               mAppend ") {"
                                               modify addScope
                                               prettyPrint s1
                                               modify removeScope
                                               newLine
                                               mAppend "}"
                                               case s2 of Skip -> return ()
                                                          _ -> do mAppend " else {"
                                                                  modify addScope
                                                                  prettyPrint s2
                                                                  modify removeScope
                                                                  newLine
                                                                  mAppend "}"
                                                                  return ()

prettyPrint (While bexpr stmt)                 = do newLine
                                                    mAppend "while"
                                                    mAppend "("
                                                    cond <- (prettyPrintB bexpr >>= dropP)
                                                    mAppend cond
                                                    mAppend ") {"
                                                    modify addScope
                                                    prettyPrint stmt
                                                    modify removeScope
                                                    newLine
                                                    mAppend "}"

prettyPrint(FCall fcexpr)                        = newLine >> prettyPrintFC fcexpr >>= mAppend
prettyPrint(OCall oCall)                         = newLine >> prettyPrintOC oCall  >>= mAppend

prettyPrint (Print a) = do newLine
                           mAppend "print("
                           prettyPrintAssignable a
                           mAppend ")"

prettyPrint (a) = newLine >> mAppend ("Not Sure about " ++ (show a))

mainPrettyPrint = do
    [filename] <- getArgs
    code <- readFile filename

    case Parsec.parse (whiteSpace >> program) filename code of
        Left e  -> print e
        Right r -> putStrLn $ snd . snd $ runState (prettyPrint r) (0, "")
--         Right r -> print r
