module Compiler(
mainCompile,
eval,
evalB,
evalObjCall,
evalFCall
)where

import DataTypes
import System.Environment
import Control.Monad
import Control.Arrow
import Control.Monad.State.Lazy
import Data.Tuple
import System.Environment
import Text.Parsec.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import DataTypes
import LanguageDef
import TokenParser
import PrettyPrinter
import ScopeEvaluator

-- -------------- ARITHMETIC EVALUATOR -------------------------------

evalABin::AExpr -> AExpr -> (Double -> Double -> a) -> MState ProgramState a
evalABin aexpr1 aexpr2 op = do
                             i1 <- evalA aexpr1
                             i2 <- evalA aexpr2
                             return $ op i1 i2

evalA :: AExpr -> MState ProgramState Double
evalA (NumericConst i) = return i
evalA (Neg aexpr)  = evalA aexpr >>= (\i-> return (-i))
evalA (ABinary Add expr1 expr2)         = evalABin expr1 expr2 (+)
evalA (ABinary Subtract expr1 expr2)    = evalABin expr1 expr2 (-)
evalA (ABinary Multiply expr1 expr2)    = evalABin expr1 expr2 (*)
evalA (ABinary Divide expr1 expr2)      = evalABin expr1 expr2 (/)
evalA (VarA s)                          = getVar s >>= (expectNumeric >>> return)
evalA (AFCall fcexpr)                   = evalFCall fcexpr >>= (expectNumeric >>> return)
evalA (AOCall obj)                      = evalObjCall obj >>= (expectNumeric >>> return)


-- -------------- BOOLEAN EVALUATOR -------------------------------

evalBBin::BExpr -> BExpr -> (Bool -> Bool -> Bool) -> MState ProgramState Bool
evalBBin bexpr1 bexpr2 op = do
                             b1 <- evalB bexpr1
                             b2 <- evalB bexpr2
                             return $ op b1 b2

evalB:: BExpr -> MState ProgramState Bool
evalB (BConst b)                            = return b
evalB (Not bexpr)                           = evalB bexpr >>= (\b -> return $ not b)
evalB (BBinary And bexpr1 bexpr2)           = evalBBin bexpr1 bexpr2 (&&)
evalB (BBinary Or bexpr1 bexpr2)            = evalBBin bexpr1 bexpr2 (||)
evalB (BCompare Greater aexpr1 aexpr2)      = evalABin aexpr1 aexpr2 (> )
evalB (BCompare GreaterE aexpr1 aexpr2)     = evalABin aexpr1 aexpr2 (>=)
evalB (BCompare Equal aexpr1 aexpr2)        = evalABin aexpr1 aexpr2 (==)
evalB (BCompare LessE aexpr1 aexpr2)        = evalABin aexpr1 aexpr2 (<=)
evalB (BCompare Less aexpr1 aexpr2)         = evalABin aexpr1 aexpr2 (< )
evalB (VarB s)                              = getVar s >>= (expectBool >>> return)
evalB (BFCall fcexpr)                       = evalFCall fcexpr >>= (expectBool >>> return)
evalB (BOCall obj)                          = evalObjCall obj >>= (expectBool >>> return)


-- -------------- FUNCTION CALL EVALUATOR -------------------------------

evalFCall::FCExpr -> MState ProgramState VariableType
evalFCall (FCExpr name params)   = do fn <- getVar name
                                      evalF (expectFn fn) params

evalF::FDExpr -> [GenericExpr] -> MState ProgramState VariableType
evalF (FDExpr parameters code) paramExpr = do appliedParams <- evalParams parameters paramExpr
                                              scopeAfterParams <- get
                                              if length scopeAfterParams == 0 then error "Global context was not found"
                                              else do
                                                   put [[], appliedParams, (last scopeAfterParams)]
                                                   eval code
                                                   newScope <- get
                                                   returned <- getVar "return"
                                                   put ((init scopeAfterParams) ++ [(last newScope)])
                                                   return returned

evalParams::[String] -> [GenericExpr] -> MState ProgramState ScopeVariables
evalParams names exprs = do appliedParams <- evalParamsInSequence (zip names exprs)
                            return (("return", Undefined):appliedParams)

evalParamsInSequence::[(String, GenericExpr)] -> MState ProgramState ScopeVariables
evalParamsInSequence [] = return []
evalParamsInSequence ((s, g):otherParams) = do value <- evalG g
                                               appliedParams <- evalParamsInSequence otherParams
                                               return $ (s, value):appliedParams


-- -------------- GENERAL EXPRESSION EVALUATOR -------------------------------
evalG::GenericExpr -> MState ProgramState VariableType
evalG (AlgebraicE aexpr)                     = evalA aexpr >>= (NumericT >>> return)
evalG (StringE str)                          = return (StrT str)
evalG (BooleanE bexpr)                       = evalB bexpr >>= (BoolT >>> return)
evalG (IdentifierE name)                     = getVar name >>= return
evalG (FunctionCallE fcexpr)                 = evalFCall fcexpr >>= return
evalG (ObjCallE obj)                         = evalObjCall obj >>= return


toStrG::AssignableE -> MState ProgramState String
toStrG a = do
           var <- evalAssignable a
           return (toString var)

-- -------------- ASSIGNMENT EVALUATOR -------------------------------

type ScopeAssigner = String -> VariableType -> ProgramState -> ProgramState
type Evaluator a = MState ProgramState a
type Caster a = a -> VariableType

evalAssign::ScopeAssigner -> String -> AssignableE -> MState ProgramState ()
evalAssign assigner name assign  = do
                                   value <- evalAssignable assign
                                   modify (assigner name value)

evalAssignV::ScopeAssigner -> String -> VariableType -> MState ProgramState ()
evalAssignV assigner name value  = modify (assigner name value)

evalAssignF::ScopeAssigner -> String -> FCExpr -> MState ProgramState ()
evalAssignF sa name fcexpr= evalFCall fcexpr >>= (evalAssignV sa name)

evalAssignable::AssignableE -> MState ProgramState VariableType
evalAssignable(ValueE    g) = evalG g
evalAssignable(FDeclare fd) = return $ FunctionT fd
evalAssignable(ODec (ObjDec vars)) = do
                                    variables <- mapM mapTuple vars
                                    return $ ObjectT variables
                                        where mapTuple t = let (s, a) = t in
                                                do val <- evalAssignable a
                                                   return (s, val)

evalChange::ValueHolder -> AssignableE -> MState ProgramState ()
evalChange (IdentVH name) assignable = evalAssign modifyVar name assignable
evalChange (ObjectVH (ObjCall o name)) assignable = do
                                        obj <- evalObjCall o
                                        value <- evalAssignable assignable
                                        let vals = do {
                                              varName <- getOriginalObjectVariable o
                                            ; strings <- getCallObjStrings o
                                            ; return (varName, strings)
                                           }
                                        case vals of Nothing -> return()
                                                     Just (varName, strings) ->
                                                        do
                                                        originalObj <- getVar varName
                                                        let newObject = alterObjWithCall (expectObject originalObj) strings name value
                                                                in modify (modifyVar varName (ObjectT newObject))
                                                        return ()


-- -------------- OBJECT EVALUATOR -----------------------------
evalObjCall::ObjCall -> MState ProgramState VariableType
evalObjCall(ObjCall o s)    = do
                              castedObj <- (evalObjCall o >>= return . expectObject)
                              guard (length castedObj >= 0)
                              case (getVariableInObject s castedObj) of
                                    Nothing -> error ("The object has no attribute called " ++ s)
                                    Just v -> return v

evalObjCall(ObjFCall o (FCExpr fName params))   = do
                                                  castedObj <- (evalObjCall o >>= return . expectObject)
                                                  guard (length castedObj >= 0)
                                                  case (getVariableInObject fName castedObj) of
                                                          Nothing -> error ("The object has no attribute called " ++ fName)
                                                          Just f -> evalF (expectFn f) params

evalObjCall(ObjFBase fcexpr)         = do
                                       var <- evalFCall fcexpr
                                       guard (length (expectObject var) >= 0)
                                       return var

evalObjCall(ObjIBase identifier)     = do
                                       var <- getVar identifier
                                       guard (length (expectObject var) >= 0)
                                       return var

getCallObjStrings::ObjCall -> Maybe [String]
getCallObjStrings (ObjCall o s) = getCallObjStrings o >>= (\r -> Just (r ++ [s]))
getCallObjStrings (ObjIBase _) = Just []
getCallObjStrings (ObjFCall _ _) = Nothing
getCallObjStrings (ObjFBase _) = Nothing

getOriginalObjectVariable::ObjCall -> Maybe String
getOriginalObjectVariable (ObjCall o _) = getOriginalObjectVariable o
getOriginalObjectVariable (ObjIBase s) = Just s
getOriginalObjectVariable (ObjFCall _ _) = Nothing
getOriginalObjectVariable (ObjFBase _) = Nothing

alterObjWithCall::[(String, VariableType)] -> [String] -> String -> VariableType -> [(String, VariableType)]
alterObjWithCall obj [] s v         = modifyInObject obj s v
alterObjWithCall obj (c:cs) s v     = let nextChild = getVariableInObject c obj in
                                      case nextChild of
                                           Nothing      -> error ("The object has no attribute called " ++ c)
                                           Just nextObj -> modifyInObject obj c (ObjectT (alterObjWithCall (expectObject nextObj) cs s v))

-- -------------- MAIN EVALUATOR -------------------------------
eval :: Stmt -> MState ProgramState ()
eval (Seq []) = return ()
eval (Seq (s:ss)) = do
                    eval (s)
                    modifiedContext <- get
                    if hasReturned modifiedContext then return () else eval (Seq ss)

eval (AssignLet name assignable)             = evalAssign  addLet name assignable
eval (AssignVar name assignable)             = evalAssign  addVar name assignable
eval (ChangeVal vh   assignable)             = evalChange  vh assignable
eval (Return expr)                           = eval (ChangeVal (IdentVH "return") expr)

eval (OCall objCall)                          = evalObjCall objCall >> return ()

eval (If bexpr s1 s2)                          = do cond <- evalB bexpr
                                                    state <- get
                                                    put ([]:state)
                                                    if cond then eval s1 else eval s2
                                                    newState <- get
                                                    put (tail newState)
                                                    return ()

eval (While bexpr stmt)                        = eval(If bexpr (Seq [stmt, (While bexpr stmt)]) Skip)

eval(FCall fcexpr)                             = evalFCall fcexpr >> return ()

eval (Print sexpr)                             = toStrG sexpr >>= liftIO . putStrLn >> return ()
eval (Skip)                                    = return ()
-- eval (a) = liftIO (print a) >> (return ())


mainCompile = do
    [filename] <- getArgs
    code <- readFile filename

    case parse (whiteSpace >> program) filename code of
        Left e  -> print e
        Right r -> runStateT (eval r) [[]] >> print "DONE"



