module Compiler(
mainCompile,
eval,
evalB
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
import ScopeEvaluator

-- -------------- ARITHMETIC EVALUATOR -------------------------------

evalABin::AExpr -> AExpr -> (Integer -> Integer -> a) -> MState ProgramState a
evalABin aexpr1 aexpr2 op = do
                             i1 <- evalA aexpr1
                             i2 <- evalA aexpr2
                             return $ op i1 i2

evalA :: AExpr -> MState ProgramState Integer
evalA (IntConst i) = return i
evalA (Neg aexpr)  = evalA aexpr >>= (\i-> return (-i))
evalA (ABinary Add expr1 expr2)         = evalABin expr1 expr2 (+)
evalA (ABinary Subtract expr1 expr2)    = evalABin expr1 expr2 (-)
evalA (ABinary Multiply expr1 expr2)    = evalABin expr1 expr2 (*)
evalA (ABinary Divide expr1 expr2)      = evalABin expr1 expr2 div
evalA (VarA s)                          = getVar s >>= (expectInt >>> return)
evalA (AFCall fcexpr)                   = evalFCall fcexpr >>= (expectInt >>> return)


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
evalG (AlgebraicE aexpr)                     = evalA aexpr >>= (IntT >>> return)
evalG (BooleanE bexpr)                       = evalB bexpr >>= (BoolT >>> return)
evalG (IdentifierE name)                     = getVar name >>= return
evalG (FunctionCallE fcexpr)                 = evalFCall fcexpr >>= return
evalG (ObjCallE obj)                         = evalObjCall obj >>= return


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
                                        case getOriginalObjectVariable o of
                                                Nothing -> return ()
                                                Just var -> let newObject = modifyInObject (expectObject obj) name value in
                                                    do
                                                    modify(modifyVar var (ObjectT newObject))
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

getVariableInObject::String -> [(String, VariableType)]-> Maybe VariableType
getVariableInObject name [] = Nothing
getVariableInObject name (s:ss) = let (oname, ovalue) = s in if name == oname then Just ovalue else getVariableInObject name ss >>= Just

getOriginalObjectVariable::ObjCall -> Maybe String
getOriginalObjectVariable(ObjCall o _)    = getOriginalObjectVariable o
getOriginalObjectVariable(ObjFCall o _)   = getOriginalObjectVariable o
getOriginalObjectVariable(ObjFBase _)          = Nothing
getOriginalObjectVariable(ObjIBase identifier) = Just identifier


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

eval (Print s) = liftIO (putStrLn s) >> return ()
eval (a) = liftIO (print a) >> (return ())


mainCompile = do
    [filename] <- getArgs
    code <- readFile filename

    case parse (whiteSpace >> program) filename code of
        Left e  -> print e
        Right r -> runStateT (eval r) [[]] >> print "DONE"



