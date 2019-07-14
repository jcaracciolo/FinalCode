module Main where

import Test.HUnit
import IntegrationTest


main = do
    putStrLn "##############################\r\nIntegration Tests\r\n##############################"
    runTestTT integrationTests