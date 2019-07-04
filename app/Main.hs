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


eval :: Scope -> IO ([ScopeVariables])
eval (Seq [], context) = return $ context
eval (Seq (s:ss), context) = do
                              newVariables <- eval (s, context)
                              eval ((Seq ss), newVariables)

eval (AssignLet name (ValueE (AlgebraicE    aexpr)), context)   = return $ addLet context name (IntT  (evalA aexpr context))
eval (AssignLet name (ValueE (BooleanE      bexpr)), context)   = return $ addLet context name (BoolT (evalB bexpr context))
eval (AssignLet name (FDeclare fdexpr), context)       = return $ addLet context name (FunctionT fdexpr)
eval (AssignLet name (ValueE (IdentifierE newName)), context)   = return $ addLet context newName  (evalVar context name)
-- eval (AssignLet name (ValueE (FunctionCallE fcexpr)), context)  = return $ addLet context name (BoolT (evalB bexpr context))

eval (AssignVar name (ValueE (AlgebraicE    aexpr)), context)   = return $ addVar context name (IntT  (evalA aexpr context))
eval (AssignVar name (ValueE (BooleanE      bexpr)), context)   = return $ addVar context name (BoolT (evalB bexpr context))
eval (AssignVar name (FDeclare fdexpr), context)       = return $ addVar context name (FunctionT fdexpr)
eval (AssignVar name (ValueE (IdentifierE newName)), context)   = return $ addVar context newName  (evalVar context name)
-- eval (AssignVar name (ValueE (FunctionCallE fcexpr)), context)  = return $ addVar context name (BoolT (evalB bexpr context))

eval (ChangeVal name (ValueE (AlgebraicE    aexpr)), context)   = return $ modifyVar context name (IntT  (evalA aexpr context))
eval (ChangeVal name (ValueE (BooleanE      bexpr)), context)   = return $ modifyVar context name (BoolT (evalB bexpr context))
eval (ChangeVal name (FDeclare fdexpr), context)       = return $ modifyVar context name (FunctionT fdexpr)
eval (ChangeVal name (ValueE (IdentifierE newName)), context)   = return $ modifyVar context newName  (evalVar context name)
-- eval (ChangeVal name (ValueE (FunctionCallE fcexpr)), context)  = return $ modifyVar context name (BoolT (evalB bexpr context))

eval ((If bexpr s1 s2), context)                        = if (evalB bexpr context)
                                                           then liftM tail (eval (s1, []:context))
                                                           else liftM tail (eval (s2, []:context))
eval ((While bexpr stmt), context)                      = if (evalB bexpr context) then
                                                              liftM tail (eval (stmt, []:context)) >>= ((,) (While bexpr stmt) >>> eval)
                                                             else eval (Skip, context)

eval(FCall (FCExpr name params), context) = let (FDExpr parameters code) = expectFn (evalVar context name) in
                                                   eval (code, context)

eval (Print s, context) = print s >> (return $ context)
eval (a, context) = print a >> (return $ context)

main = do
    [filename] <- getArgs
    code <- readFile filename

    case parse program filename code of
        Left e  -> print e
        Right r -> eval (r, []) >> print "Done"
