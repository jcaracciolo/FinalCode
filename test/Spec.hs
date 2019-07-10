module Spec(
main
)where

import Test.HUnit
import CompilerTest

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
-- main :: IO Counts
main = runTestTT compilerTests