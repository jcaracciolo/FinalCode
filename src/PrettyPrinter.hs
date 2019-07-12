module PrettyPrinter(
mainPrettyPrint,
prettyPrintFD,
prettyPrintOD,
toString
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
type Precedence = Int
type PrettyReturn = (String, Precedence) -- String | Precedence

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

wrap::Precedence -> PrettyReturn -> String
wrap p (s,i) = if i>p then "(" ++ s ++ ")" else s

dropP::PrettyReturn -> State PrinterState String
dropP (s,i) = return s

-------   Algebraic Pretty Print

prettyPrintABin::AExpr -> AExpr -> String -> Precedence -> State PrinterState PrettyReturn
prettyPrintABin aexpr1 aexpr2 op p = do
                             i1 <- prettyPrintA aexpr1
                             i2 <- prettyPrintA aexpr2
                             return $ ((wrap p i1) ++ op ++ (wrap p i2), p)

prettyPrintA::AExpr -> State PrinterState PrettyReturn
prettyPrintA (NumericConst i)               = return $ (show i, -1)
prettyPrintA (Neg aexpr)                    = prettyPrintA aexpr >>= \s -> return $ ("-" ++ (wrap 0 s), 0)
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
                             return $ ((wrap p i1) ++ op ++ (wrap p i2), p)

prettyPrintB:: BExpr -> State PrinterState PrettyReturn
prettyPrintB (BConst b)                           = return $ (show b, -1)
prettyPrintB (Not bexpr)                          = prettyPrintB bexpr >>= \s -> return $ ("not " ++ wrap 0 s, 0)
prettyPrintB (BBinary And bexpr1 bexpr2)          = prettyPrintBBin bexpr1 bexpr2 " && " 4
prettyPrintB (BBinary Or bexpr1 bexpr2)           = prettyPrintBBin bexpr1 bexpr2 " || " 4
prettyPrintB (BCompare Greater aexpr1 aexpr2)     = prettyPrintABin aexpr1 aexpr2 " > " 3
prettyPrintB (BCompare GreaterE aexpr1 aexpr2)    = prettyPrintABin aexpr1 aexpr2 " >= " 3
prettyPrintB (BCompare Equal aexpr1 aexpr2)       = prettyPrintABin aexpr1 aexpr2 " == " 3
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
prettyPrintG (StringE s)                            = return $ show s

-------- ObjectCall Pretty Print
prettyPrintOC::ObjCall -> State PrinterState String
prettyPrintOC (ObjCall o s)     = prettyPrintOC o >>= (\os -> return $ os ++ "." ++ s)
prettyPrintOC (ObjFCall o f)    = do os <- prettyPrintOC o
                                     fs <- prettyPrintFC f
                                     return $ os ++ "." ++ fs
prettyPrintOC (ObjFBase f)      = prettyPrintFC f
prettyPrintOC (ObjIBase s)      = return s


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
                             mAppend (toString a)
                             prettyPrintOTVs vs
                             return ()

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
