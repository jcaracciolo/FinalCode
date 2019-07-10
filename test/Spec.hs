module Main where

import Test.HUnit
import CompilerTest
import ScopeEvaluatorTest


main = do
    putStrLn "Compiler Tests:"
    runTestTT compilerTests
    putStrLn "ScopeEvaluator Tests:"
    runTestTT scopeEvaluatorTests