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
evalAssignF sa name fcexpr= evalFCall fcexpr >>= (evalAssignV sa name)

-- -------------- MAIN EVALUATOR -------------------------------
eval :: Stmt -> MState ProgramState ()
eval (Seq []) = return ()
eval (Seq (s:ss)) = do
                    eval (s)
                    modifiedContext <- get
                    if hasReturned modifiedContext then return () else eval (Seq ss)

eval (AssignLet name (ValueE (AlgebraicE    aexpr)))   = evalAssign  addLet name (evalA aexpr) IntT
eval (AssignLet name (ValueE (BooleanE      bexpr)))   = evalAssign  addLet name (evalB bexpr) BoolT
eval (AssignLet name (ValueE (IdentifierE oldName)))   = evalAssign  addLet name (getVar oldName) id
eval (AssignLet name (FDeclare fdexpr))                = evalAssignV addLet name (FunctionT fdexpr)
eval (AssignLet name (ValueE (FunctionCallE fcexpr)))  = evalAssignF addLet name fcexpr

eval (AssignVar name (ValueE (AlgebraicE    aexpr)))   = evalAssign  addVar name (evalA aexpr) IntT
eval (AssignVar name (ValueE (BooleanE      bexpr)))   = evalAssign  addVar name (evalB bexpr) BoolT
eval (AssignVar name (ValueE (IdentifierE oldName)))   = evalAssign  addVar name (getVar oldName) id
eval (AssignVar name (FDeclare fdexpr))                = evalAssignV addVar name (FunctionT fdexpr)
eval (AssignVar name (ValueE (FunctionCallE fcexpr)))  = evalAssignF addVar name fcexpr

eval (ChangeVal name (ValueE (AlgebraicE    aexpr)))   = evalAssign  modifyVar name (evalA aexpr) IntT
eval (ChangeVal name (ValueE (BooleanE      bexpr)))   = evalAssign  modifyVar name (evalB bexpr) BoolT
eval (ChangeVal name (ValueE (IdentifierE oldName)))   = evalAssign  modifyVar name (getVar oldName) id
eval (ChangeVal name (FDeclare fdexpr))                = evalAssignV modifyVar name (FunctionT fdexpr)
eval (ChangeVal name (ValueE (FunctionCallE fcexpr)))  = evalAssignF modifyVar name fcexpr

eval (Return expr)                                     = eval (ChangeVal "return" expr)

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



