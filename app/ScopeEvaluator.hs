module ScopeEvaluator(
expectInt,
expectBool,
expectStr,
expectFn,
evalVar,
addVar,
addLet,
modifyVar,
hasReturned,
) where

import DataTypes
import Control.Arrow
import Control.Exception

isDefined:: ScopeVariables -> String -> Bool
isDefined [] name = False
isDefined (v:vs) name = fst v == name || isDefined vs name


failIfDefined::ScopeVariables -> String -> ProgramState -> ProgramState
failIfDefined s name result = if isDefined s name then error ("Variable " ++ name ++ " is already defined") else result

-- Given a ScopeVariables, if the variable name is inside the scope, then its value is modified to value
-- And the new ScopeVariables are returned, else Nothing
modifyScope:: ScopeVariables -> String -> VariableType -> Maybe ScopeVariables
modifyScope [] _ _ = Nothing
modifyScope (v:vs) name value = if (fst v) == name
                                then Just ((name, value):vs)
                                else (modifyScope vs name value) >>= ((v:) >>> Just)

-- Given a list of ScopeVariables, if the variable name is inside any scope, its value is modified to value
-- And the new list of ScopeVariables is returned, else the variable is added to the second scope
modifyVar::ProgramState -> String -> VariableType -> ProgramState
modifyVar [] name value = [[(name, value)], []] -- This is the first variable of the whole program and the global scope was not created
modifyVar [gs] name value = case modifyScope gs name value of Nothing -> [(name, value):gs]
                                                              Just scope -> [scope]

modifyVar (s:ss) name value = case modifyScope s name value of Nothing -> s : (modifyVar ss name value)
                                                               Just scope -> scope:ss

-- Given a list of ScopeVariables, a new variable name is added to the first scope with the value
addLet::ProgramState -> String -> VariableType -> ProgramState
addLet [] name value     = [[(name, value)]]
addLet (s:ss) name value = failIfDefined s name (((name, value):s):ss)

-- Given a list of ScopeVariables, a new variable of function scope is added to the corresponding scope variables, or throws error if already defined.
addVar::ProgramState -> String -> VariableType -> ProgramState
addVar [] name value        = [[(name, value)]]
addVar [gs] name value      = failIfDefined gs name [(name, value):gs]
addVar [fs, gs] name value  = failIfDefined fs name [((name, value):fs), gs]
addVar (s:ss) name value    = failIfDefined s name  (s:(addVar ss name value))

-- Given a list of ScopeVariables, a new variable of global scope is added to the corresponding scope variables, or throws error if already defined.
addGlobal::ProgramState -> String -> VariableType -> ProgramState
addGlobal [] name value     = [[(name, value)]]
addGlobal [gs] name value   = failIfDefined gs name [(name, value):gs]
addGlobal (s:ss) name value = failIfDefined s  name (s:(addGlobal ss name value))

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
expectInt:: VariableType -> Integer
expectInt (IntT i) = i
expectInt t = error ("Expected Integer in but got " ++ (show t))

expectBool:: VariableType -> Bool
expectBool (BoolT b) = b
expectBool t = error ("Expected Boolean in but got " ++ (show t))

expectStr:: VariableType ->  String
expectStr (StrT s) = s
expectStr t = error ("Expected String in but got " ++ (show t))

expectFn:: VariableType ->  FDExpr
expectFn (FunctionT f) = f
expectFn t = error ("Expected String in but got " ++ (show t))

hasReturned::ProgramState -> Bool
hasReturned [] = False
hasReturned (s:ss) = case evalInScope s "return" of Nothing -> hasReturned ss
                                                    Just Undefined -> False
                                                    Just _ -> True

