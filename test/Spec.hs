module Main where

import Test.HUnit
import IntegrationTest
import ScopeEvaluatorTest


main = do
    putStrLn "##############################\r\nIntegration Tests\r\n##############################"
    runTestTT integrationTests
    putStrLn "ScopeEvaluator Tests:"
    runTestTT scopeEvaluatorTests