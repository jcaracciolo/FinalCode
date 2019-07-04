import System.IO
import System.Environment
import Control.Monad
import Control.Arrow
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

evalA :: AExpr -> [ScopeVariables] -> Integer
evalA (IntConst i) vars                     = i
evalA (Neg expr) vars                       = - (evalA expr vars)
evalA (ABinary Add expr1 expr2) vars        = (evalA expr1 vars) +       (evalA expr2 vars)
evalA (ABinary Subtract expr1 expr2) vars   = (evalA expr1 vars) -       (evalA expr2 vars)
evalA (ABinary Multiply expr1 expr2) vars   = (evalA expr1 vars) *       (evalA expr2 vars)
evalA (ABinary Divide expr1 expr2) vars     = (evalA expr1 vars) `div`   (evalA expr2 vars)
evalA (VarA s) vars                         = expectInt (evalVar vars s)

evalB:: BExpr -> [ScopeVariables] -> Bool
evalB (BConst b) vars                           = b
evalB (Not bexpr) vars                          = not (evalB bexpr vars)
evalB (BBinary And expr1 expr2) vars            = (evalB expr1 vars) && (evalB expr2 vars)
evalB (BBinary Or expr1 expr2)  vars            = (evalB expr1 vars) || (evalB expr2 vars)
evalB (BCompare Greater aexpr1 aexpr2)  vars    = (evalA aexpr1 vars) > (evalA aexpr2 vars)
evalB (BCompare GreaterE aexpr1 aexpr2) vars    = (evalA aexpr1 vars) >= (evalA aexpr2 vars)
evalB (BCompare Equal aexpr1 aexpr2) vars       = (evalA aexpr1 vars) == (evalA aexpr2 vars)
evalB (BCompare LessE aexpr1 aexpr2) vars       = (evalA aexpr1 vars) <= (evalA aexpr2 vars)
evalB (BCompare Less aexpr1 aexpr2) vars        = (evalA aexpr1 vars) < (evalA aexpr2 vars)
evalB (VarB s) vars                             = expectBool (evalVar vars s)

evalF::FDExpr -> [GenericExpr] -> [ScopeVariables] -> IO(([ScopeVariables], VariableType))
evalF (FDExpr parameters code) paramExpr []      = error "Global context was not found"
evalF (FDExpr parameters code) paramExpr context = do
                                                   (scopeAfterParams, appliedParams) <- evalParams parameters paramExpr context
                                                   newScope <- eval (code, [ appliedParams, (last scopeAfterParams) ])
                                                   newGlobalScope <- return $ (init scopeAfterParams) ++ [(last newScope)]
                                                   return $ (newGlobalScope, (evalVar newScope "return"))

evalParamsInSequence::[(String, GenericExpr)] -> [ScopeVariables] -> IO(([ScopeVariables], ScopeVariables))
evalParamsInSequence [] context = return (context, [])
evalParamsInSequence ((s, g):otherParams) context = do
                                                    (newContext, value) <- evalG g context
                                                    (finalContext, appliedParams) <- evalParamsInSequence otherParams newContext
                                                    return $ (finalContext, (s, value):appliedParams)

evalParams::[String] -> [GenericExpr] -> [ScopeVariables] -> IO(([ScopeVariables], ScopeVariables))
evalParams names exprs context = do
                                 (newContext, appliedParams) <- evalParamsInSequence (zip names exprs) context
                                 return $ (newContext, ("return", Undefined):appliedParams)

evalG::GenericExpr -> [ScopeVariables] -> IO(([ScopeVariables], VariableType))
evalG (AlgebraicE aexpr) vars                     = return $ (vars, IntT (evalA aexpr vars))
evalG (BooleanE bexpr) vars                       = return $ (vars, BoolT (evalB bexpr vars))
evalG (IdentifierE name) vars                     = return $ (vars, evalVar vars name)
evalG (FunctionCallE (FCExpr name params)) vars   = evalF (expectFn (evalVar vars name)) params vars


evalAssign::([ScopeVariables] -> String -> VariableType -> [ScopeVariables])
                -> [ScopeVariables] -> String -> VariableType -> IO ([ScopeVariables])
evalAssign f context name value = return $ f context name value

evalAssignF::([ScopeVariables] -> String -> VariableType -> [ScopeVariables])
                -> [ScopeVariables] -> String -> FCExpr -> IO ([ScopeVariables])
evalAssignF f context name (FCExpr fName params) = do
                             (newContext, returnValue) <- (evalF (expectFn (evalVar context fName)) params context)
                             return $ f newContext name returnValue

eval :: Scope -> IO ([ScopeVariables])
eval (Seq [], context) = return $ context
eval (Seq (s:ss), context) = do
                              newVariables <- eval (s, context)
                              eval ((Seq ss), newVariables)




eval (AssignLet name (ValueE (AlgebraicE    aexpr)), context)   = evalAssign addLet context name (IntT  (evalA aexpr context))
eval (AssignLet name (ValueE (BooleanE      bexpr)), context)   = evalAssign addLet context name (BoolT (evalB bexpr context))
eval (AssignLet name (FDeclare fdexpr), context)                = evalAssign addLet context name (FunctionT fdexpr)
eval (AssignLet name (ValueE (IdentifierE oldName)), context)   = evalAssign addLet context name (evalVar context oldName)
eval (AssignLet name (ValueE (FunctionCallE fcExpr)), context)  = evalAssignF addLet context name fcExpr


eval (AssignVar name (ValueE (AlgebraicE    aexpr)), context)   = evalAssign addVar context name (IntT  (evalA aexpr context))
eval (AssignVar name (ValueE (BooleanE      bexpr)), context)   = evalAssign addVar context name (BoolT (evalB bexpr context))
eval (AssignVar name (FDeclare fdexpr), context)                = evalAssign addVar context name (FunctionT fdexpr)
eval (AssignVar name (ValueE (IdentifierE oldName)), context)   = evalAssign addVar context name (evalVar context oldName)
eval (AssignVar name (ValueE (FunctionCallE fcExpr)), context)  = evalAssignF addVar context name fcExpr

eval (ChangeVal name (ValueE (AlgebraicE    aexpr)), context)   = evalAssign modifyVar context name (IntT  (evalA aexpr context))
eval (ChangeVal name (ValueE (BooleanE      bexpr)), context)   = evalAssign modifyVar context name (BoolT (evalB bexpr context))
eval (ChangeVal name (FDeclare fdexpr), context)                = evalAssign modifyVar context name (FunctionT fdexpr)
eval (ChangeVal name (ValueE (IdentifierE oldName)), context)   = evalAssign modifyVar context name (evalVar context oldName)
eval (ChangeVal name (ValueE (FunctionCallE fcExpr)), context)  = evalAssignF modifyVar context name fcExpr


eval ((If bexpr s1 s2), context)                        = if (evalB bexpr context)
                                                           then liftM tail (eval (s1, []:context))
                                                           else liftM tail (eval (s2, []:context))
eval ((While bexpr stmt), context)                      = if (evalB bexpr context) then
                                                              liftM tail (eval (stmt, []:context)) >>= ((,) (While bexpr stmt) >>> eval)
                                                             else eval (Skip, context)

eval(FCall (FCExpr name params), context) = do
                                            (newContext, returnValue) <- evalF (expectFn (evalVar context name)) params context
                                            return newContext

eval (Print s, context) = print s >> (return $ context)
eval (a, context) = print a >> (return $ context)

main = do
    [filename] <- getArgs
    code <- readFile filename

    case parse program filename code of
        Left e  -> print e
        Right r -> eval (r, [[]]) >> print "Done"
