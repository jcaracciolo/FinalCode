module ScopeEvaluator(
expectNumeric,
expectBool,
expectStr,
expectFn,
expectObject,
addVar,
addLet,
modifyVar,
getVar,
hasReturned,
modifyInObject,
getVariableInObject,
evalVar,
) where

import DataTypes
import Control.Arrow
import Control.Exception
import Control.Monad.State.Lazy
import PrettyPrinter

isDefined::String -> ScopeVariables -> Bool
isDefined name  []      = False
isDefined name  (v:vs)  = fst v == name || isDefined name vs

failIfDefined::String -> ScopeVariables -> ProgramState -> ProgramState
failIfDefined name s result = if isDefined name s then error ("Variable " ++ name ++ " is already defined") else result

-- Given a ScopeVariables, if the variable name is inside the scope, then its value is modified to value
-- And the new ScopeVariables are returned, else Nothing
modifyScope:: ScopeVariables -> String -> VariableType -> Maybe ScopeVariables
modifyScope [] _ _ = Nothing
modifyScope (v:vs) name value = if (fst v) == name
                                then Just ((name, value):vs)
                                else (modifyScope vs name value) >>= ((v:) >>> Just)

-- Given a list of ScopeVariables, if the variable name is inside any scope, its value is modified to value
-- And the new list of ScopeVariables is returned, else the variable is added to the second scope
modifyVar::String -> VariableType -> ProgramState ->  ProgramState
modifyVar name value []     = [[(name, value)], []] -- This is the first variable of the whole program and the global scope was not created
modifyVar name value [gs]   = case modifyScope gs name value of Nothing -> [(name, value):gs]
                                                                Just scope -> [scope]

modifyVar name value (s:ss) = case modifyScope s name value of Nothing -> s : (modifyVar name value ss)
                                                               Just scope -> scope:ss

-- Given a list of ScopeVariables, a new variable name is added to the first scope with the value
addLet:: String -> VariableType -> ProgramState -> ProgramState
addLet name value []      = [[(name, value)]]
addLet name value (s:ss)  = failIfDefined name s (((name, value):s):ss)

-- Given a list of ScopeVariables, a new variable of function scope is added to the corresponding scope variables, or throws error if already defined.
addVar::String -> VariableType -> ProgramState ->  ProgramState
addVar name value []        = [[(name, value)]]
addVar name value [gs]      = failIfDefined name gs [(name, value):gs]
addVar name value [fs, gs]  = failIfDefined name fs [((name, value):fs), gs]
addVar name value (s:ss)    = failIfDefined name s  (s:(addVar name value ss))

-- Given a list of ScopeVariables, a new variable of global scope is added to the corresponding scope variables, or throws error if already defined.
addGlobal::String -> VariableType -> ProgramState ->  ProgramState
addGlobal name value [] = [[(name, value)]]
addGlobal name value [gs] = failIfDefined name gs [(name, value):gs]
addGlobal name value (s:ss) = failIfDefined name s (s:(addGlobal name value ss))

-- Evaluates a variable name in the ScopesVariable, if is not found, Nothing is returned
evalInScope:: ScopeVariables -> String -> Maybe VariableType
evalInScope [] _ = Nothing
evalInScope (s:ss) name = let (orName, value) = s in if orName == name
                                                    then Just value
                                                    else evalInScope ss name

-- Evaluates a variable in a list of ScopeVariables, giving priority to the firsts scopes
-- If the variable does not exist in any scope, a runtime error is thrown
evalVar:: ProgramState -> String -> VariableType
evalVar [] name = error ("Variable " ++ name ++ " is not defined in the scope")
evalVar (s:ss) name = case evalInScope s name of Nothing -> evalVar ss name
                                                 Just value -> value
getVar::String -> MState ProgramState VariableType
getVar name = do
              state <- get
              return $ evalVar state name

modifyInObject:: [(String, VariableType)] -> String -> VariableType -> [(String, VariableType)]
modifyInObject [] name value= [(name, value)]
modifyInObject (s:ss) name value = if fst s== name then (name, value):ss else s:(modifyInObject ss name value)

getVariableInObject::String -> [(String, VariableType)]-> Maybe VariableType
getVariableInObject name [] = Nothing
getVariableInObject name (s:ss) = let (oname, ovalue) = s in if name == oname then Just ovalue else getVariableInObject name ss >>= Just


expectNumeric:: VariableType -> Double
expectNumeric (NumericT i) = i
expectNumeric t = error ("Expected Integer in but got " ++ (show t))

expectBool:: VariableType -> Bool
expectBool (BoolT b) = b
expectBool t = error ("Expected Boolean in but got " ++ (show t))

expectStr:: VariableType ->  String
expectStr (StrT s) = s
expectStr t = error ("Expected String in but got " ++ (show t))

expectFn:: VariableType ->  FDExpr
expectFn (FunctionT f) = f
expectFn t = error ("Expected String in but got " ++ (show t))

expectObject:: VariableType ->  [(String, VariableType)]
expectObject (ObjectT elems) = elems
expectObject t = error ("Expected Object in but got " ++ (show t))

hasReturned::ProgramState -> Bool
hasReturned [] = False
hasReturned (s:ss) = case evalInScope s "return" of Nothing -> hasReturned ss
                                                    Just Undefined -> False
                                                    Just _ -> True
