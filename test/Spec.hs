module Main where

import Test.HUnit
import IntegrationTest
import ScopeEvaluatorTest


main = do
    putStrLn "##############################\r\nIntegration Tests\r\n##############################"
    runTestTT integrationTests
    putStrLn "##############################\r\nScopeEvaluator Tests\r\n##############################"
    runTestTT scopeEvaluatorTests