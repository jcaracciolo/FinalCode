module ScopeEvaluator(
expectInt,
expectBool,
expectStr,
evalVar,
addVar,
modifyVar,
) where

import DataTypes
import Control.Arrow

-- Given a ScopeVariables, if the variable name is inside the scope, then its value is modified to value
-- And the new ScopeVariables are returned, else Nothing
modifyScope:: ScopeVariables -> String -> VariableType -> Maybe ScopeVariables
modifyScope [] _ _ = Nothing
modifyScope (v:vs) name value = if (fst v) == name
                                then Just ((name, value):vs)
                                else (modifyScope vs name value) >>= ((v:) >>> Just)


-- Given a list of ScopeVariables, if the variable name is inside any scope, its value is modified to value
-- And the new list of ScopeVariables is returned, else Nothing
modifyVarIfAvailable:: [ScopeVariables] -> String -> VariableType -> Maybe [ScopeVariables]
modifyVarIfAvailable [] _ _ = Nothing
modifyVarIfAvailable (s:ss) name value = case modifyScope s name value of
                                            Nothing -> (modifyVarIfAvailable ss name value) >>= ((s:) >>> Just)
                                            Just scope -> Just (scope:ss)

-- Given a list of ScopeVariables, if the variable name is inside any scope, its value is modified to value
-- And the new list of ScopeVariables is returned, else the variable is added to the first scope
modifyVar::[ScopeVariables] -> String -> VariableType -> [ScopeVariables]
modifyVar [] name value = [[(name, value)]]
modifyVar (s:ss) name value = case modifyVarIfAvailable (s:ss) name value of Nothing -> (((name, value):s):ss)
                                                                             Just scope -> scope

-- Given a list of ScopeVariables, a new variable name is added to the first scope with the value
-- If the variable name already exists, a runtime error is thrown
addVar::[ScopeVariables] -> String -> VariableType -> [ScopeVariables]
addVar [] name value = [[(name, value)]]
addVar (s:ss) name value = case modifyScope s name value of Nothing -> (((name, value):s):ss)
                                                            Just _ -> error ("Variable " ++ name ++ " is already defined")

-- Evaluates a variable name in the ScopesVariable, if is not found, Nothing is returned
evalInScope:: ScopeVariables -> String -> Maybe VariableType
evalInScope [] _ = Nothing
evalInScope (s:ss) name = let (orName, value) = s in if orName == name
                                                    then Just value
                                                    else evalInScope ss name

-- Evaluates a variable in a list of ScopeVariables, giving priority to the firsts scopes
-- If the variable does not exist in any scope, a runtime error is thrown
evalVar:: [ScopeVariables] -> String -> VariableType
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
