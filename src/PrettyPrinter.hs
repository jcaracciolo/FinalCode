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

wrap::String -> String
wrap s = "(" ++ s ++ ")"

-------   Algebraic Pretty Print

prettyPrintABin::AExpr -> AExpr -> String -> State PrinterState String
prettyPrintABin aexpr1 aexpr2 op = do
                             i1 <- prettyPrintA aexpr1
                             i2 <- prettyPrintA aexpr2
                             return $ (wrap i1) ++ op ++ (wrap i2)

prettyPrintA::AExpr -> State PrinterState String
prettyPrintA (NumericConst i)                   = return $ show i
prettyPrintA (Neg aexpr)                    = prettyPrintA aexpr >>= \s -> return $ "-" ++ (wrap s)
prettyPrintA (ABinary Add expr1 expr2)      = prettyPrintABin expr1 expr2 " + "
prettyPrintA (ABinary Subtract expr1 expr2) = prettyPrintABin expr1 expr2 " - "
prettyPrintA (ABinary Multiply expr1 expr2) = prettyPrintABin expr1 expr2 " * "
prettyPrintA (ABinary Divide expr1 expr2)   = prettyPrintABin expr1 expr2 " / "
prettyPrintA (VarA s)                       = return s
prettyPrintA (AFCall fcexpr)                = prettyPrintFC fcexpr >>= (wrap >>> return)

-------- Boolean Pretty Print

prettyPrintBBin::BExpr -> BExpr -> String -> State PrinterState String
prettyPrintBBin bexpr1 bexpr2 op = do
                             i1 <- prettyPrintB bexpr1
                             i2 <- prettyPrintB bexpr2
                             return $ (wrap i1) ++ op ++ (wrap i2)

prettyPrintB:: BExpr -> State PrinterState String
prettyPrintB (BConst b)                           = return $ show b
prettyPrintB (Not bexpr)                          = prettyPrintB bexpr >>= \s -> return $ "not " ++ wrap(s)
prettyPrintB (BBinary And bexpr1 bexpr2)          = prettyPrintBBin bexpr1 bexpr2 " && "
prettyPrintB (BBinary Or bexpr1 bexpr2)           = prettyPrintBBin bexpr1 bexpr2 " || "
prettyPrintB (BCompare Greater aexpr1 aexpr2)     = prettyPrintABin aexpr1 aexpr2 " > "
prettyPrintB (BCompare GreaterE aexpr1 aexpr2)    = prettyPrintABin aexpr1 aexpr2 " >= "
prettyPrintB (BCompare Equal aexpr1 aexpr2)       = prettyPrintABin aexpr1 aexpr2 " == "
prettyPrintB (BCompare LessE aexpr1 aexpr2)       = prettyPrintABin aexpr1 aexpr2 " <= "
prettyPrintB (BCompare Less aexpr1 aexpr2)        = prettyPrintABin aexpr1 aexpr2 " < "
prettyPrintB (VarB s)                             = return s
prettyPrintB (BFCall fcexpr)                      = prettyPrintFC fcexpr >>= (wrap >>> return)

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
prettyPrintG (AlgebraicE aexpr)                     = prettyPrintA aexpr
prettyPrintG (BooleanE bexpr)                       = prettyPrintB bexpr
prettyPrintG (IdentifierE name)                     = return name
prettyPrintG (FunctionCallE fcexpr)                 = prettyPrintFC fcexpr
prettyPrintG (ObjCallE oCall)                       = prettyPrintOC oCall
prettyPrintG (StringE s)                            = return s

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
toString (StrT s)                                   = s
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
                                               cond <- prettyPrintB bexpr
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
                                                    cond <- prettyPrintB bexpr
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
