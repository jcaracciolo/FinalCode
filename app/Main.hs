import System.IO
import System.Environment
import Control.Monad
import Control.Arrow
import Control.Monad.State.Lazy
import Data.Tuple
import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import DataTypes
import LanguageDef
import TokenParser
import ScopeEvaluator

-- -------------- ARITMETIC EVALUATOR -------------------------------

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
evalA (VarA s)                          = getVar s >>= (\i -> return $ expectInt (i))


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
evalB (VarB s)                              = getVar s >>= (\b -> return $ expectBool (b))


-- -------------- FUNCTION CALL EVALUATOR -------------------------------
evalF::FDExpr -> [GenericExpr] -> MState ProgramState VariableType
evalF (FDExpr parameters code) paramExpr = do
                                           appliedParams <- evalParams parameters paramExpr
                                           scopeAfterParams <- get
                                           if length scopeAfterParams == 0 then error "Global context was not found"
                                           else do
                                                put [appliedParams, (last scopeAfterParams)]
                                                eval code
                                                newScope <- get
                                                put ((init scopeAfterParams) ++ [(last newScope)])
                                                returned <- getVar "return"
                                                return returned

evalParams::[String] -> [GenericExpr] -> MState ProgramState ScopeVariables
evalParams names exprs = do appliedParams <- evalParamsInSequence (zip names exprs)
                            newContext <- get
                            return (("return", Undefined):appliedParams)

evalParamsInSequence::[(String, GenericExpr)] -> MState ProgramState ScopeVariables
evalParamsInSequence [] = return []
evalParamsInSequence ((s, g):otherParams) = do value <- evalG g
                                               appliedParams <- evalParamsInSequence otherParams
                                               return $ (s, value):appliedParams
--


-- -------------- GENERAL EXPRESSION EVALUATOR -------------------------------
evalG::GenericExpr -> MState ProgramState VariableType
evalG (AlgebraicE aexpr)                     = evalA aexpr >>= (IntT >>> return)
evalG (BooleanE bexpr)                       = evalB bexpr >>= (BoolT >>> return)
evalG (IdentifierE name)                     = getVar name >>= return
evalG (FunctionCallE (FCExpr name params))   = do fn <- getVar name
                                                  evalF (expectFn fn) params

-- -------------- ASSIGNMENT EVALUATOR -------------------------------

type ScopeAssigner = String -> VariableType -> ProgramState -> ProgramState
type Evaluator a = MState ProgramState a
type Caster a = a -> VariableType

evalAssign::ScopeAssigner -> String -> Evaluator a -> Caster a -> MState ProgramState ()
evalAssign assigner name evaluator caster  = do
                                            value <- evaluator
                                            modify (assigner name (caster value))

evalAssignV::ScopeAssigner -> String -> VariableType -> MState ProgramState ()
evalAssignV assigner name value  = modify (assigner name value)

evalAssignF::ScopeAssigner -> String -> FCExpr -> MState ProgramState ()
evalAssignF sa name (FCExpr fName params) = do
                                            fn <- getVar fName
                                            returned <- evalF (expectFn fn) params
                                            evalAssignV sa name returned

-- -------------- MAIN EVALUATOR -------------------------------
eval :: Stmt -> MState ProgramState ()
eval (Seq []) = return ()
eval (Seq (s:ss)) = do
                    context <- get
                    eval (s)
                    modifiedContext <- get
                    if hasReturned modifiedContext then return () else eval (Seq ss)

eval (AssignLet name (ValueE (AlgebraicE    aexpr)))   = evalAssign  addLet name (evalA aexpr) IntT
eval (AssignLet name (ValueE (BooleanE      bexpr)))   = evalAssign  addLet name (evalB bexpr) BoolT
eval (AssignLet name (ValueE (IdentifierE oldName)))   = evalAssign  addLet name (getVar oldName) id
eval (AssignLet name (FDeclare fdexpr))                = evalAssignV  addLet name (FunctionT fdexpr)
eval (AssignLet name (ValueE (FunctionCallE (FCExpr fName params))))  = do declaration <- getVar fName
                                                                           returned <- evalF (expectFn declaration) params
                                                                           evalAssignV addLet name returned

eval (AssignVar name (ValueE (AlgebraicE    aexpr)))   = evalAssign  addVar name (evalA aexpr) IntT
eval (AssignVar name (ValueE (BooleanE      bexpr)))   = evalAssign  addVar name (evalB bexpr) BoolT
eval (AssignVar name (ValueE (IdentifierE oldName)))   = evalAssign  addVar name (getVar oldName) id
eval (AssignVar name (FDeclare fdexpr))                = evalAssignV  addVar name (FunctionT fdexpr)
eval (AssignVar name (ValueE (FunctionCallE (FCExpr fName params))))  = do declaration <- getVar fName
                                                                           returned <- evalF (expectFn declaration) params
                                                                           evalAssignV addVar name returned

eval (ChangeVal name (ValueE (AlgebraicE    aexpr)))   = evalAssign  modifyVar name (evalA aexpr) IntT
eval (ChangeVal name (ValueE (BooleanE      bexpr)))   = evalAssign  modifyVar name (evalB bexpr) BoolT
eval (ChangeVal name (ValueE (IdentifierE oldName)))   = evalAssign  modifyVar name (getVar oldName) id
eval (ChangeVal name (FDeclare fdexpr))                = evalAssignV  modifyVar name (FunctionT fdexpr)
eval (ChangeVal name (ValueE (FunctionCallE (FCExpr fName params))))  = do declaration <- getVar fName
                                                                           returned <- evalF (expectFn declaration) params
                                                                           evalAssignV modifyVar name returned

eval ((If bexpr s1 s2))                        = do cond <- evalB bexpr
                                                    state <- get
                                                    put ([]:state)
                                                    if cond then eval s1 else eval s2
                                                    newState <- get
                                                    put (tail newState)
                                                    return ()

eval ((While bexpr stmt))                      = eval(If bexpr (Seq [stmt, (While bexpr stmt)]) Skip)

eval(FCall (FCExpr name params)) =  do declaration <- getVar name
                                       _ <- evalF (expectFn declaration) params
                                       return ()

eval (Print s) = liftIO (print s) >> return ()
eval (a) = liftIO (print a) >> (return ())

main = do
    [filename] <- getArgs
    code <- readFile filename

    case parse program filename code of
        Left e  -> print e
        Right r -> runStateT (eval r) [[]] >> print "DONE"
