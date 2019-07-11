module CompilerTest(
compilerTests
) where

import Test.HUnit
import Compiler
import DataTypes
import Control.Monad.State.Lazy
import TokenParser
import LanguageDef
import Text.ParserCombinators.Parsec
import ScopeEvaluator
import Control.Exception


compilerTests = TestList [
                    TestLabel "IF/ELSE statement" testIfElse,
                    TestLabel "WHILE statement" testWhile,
                    TestLabel "Bool && Arithmetic" testBoolAndArithmetic,
                    TestLabel "Let variable in scope" testLetVariableInScope,
                    TestLabel "Let variable not in scope" testLetVariableNotInScope,
                    TestLabel "Var variable in scope" testVarVariableInScope,
                    TestLabel "Var variable not in scope" testVarVariableNotInScope,
                    TestLabel "Global variable visible from block" testGlobalVariableInsideBlock,
                    TestLabel "Global variable visible from function" testGlobalVariableInsideFunction
                    ]

-- UTILS

assertError::SomeException -> Assertion
assertError _ = assertBool "Exception was thrown" True

-- END OF UTILS

test2 = TestCase (
         do
            code <- readFile "test.js"
            catch (

                case parse (whiteSpace >> program) "" code of
                     Left e  -> assertBool "" False
                     Right r -> do
                                (_, finalState) <- runStateT (eval r) [[]]
                                assertBool "Exception was not thrown" False
                  ) assertError
        )


testIfElse = TestCase (
         do code <- readFile "test/testIfElse.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )

testWhile = TestCase (
         do code <- readFile "test/testWhile.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )


-- TODO: Make this test catch the error
testBoolAndArithmetic = TestCase (
         do code <- readFile "test/testBoolAndArithmetic.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 2" (IntT 2) (evalVar finalState "ans"))
        )

testLetVariableInScope = TestCase (
         do code <- readFile "test/testLetVariableInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )

-- TODO: Make this test catch the error
testLetVariableNotInScope = TestCase (
         do code <- readFile "test/testLetVariableNotInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 12" (IntT 12) (evalVar finalState "ans"))
--                             (assertEqual "ans equals 1" 2 (evalVar finalState "ans"))
         )

testVarVariableInScope = TestCase (
         do code <- readFile "test/testVarVariableInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )

-- TODO: Make this test catch the error
testVarVariableNotInScope = TestCase (
         do code <- readFile "test/testVarVariableNotInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )

testGlobalVariableInsideBlock = TestCase (
         do code <- readFile "test/testGlobalVariableInsideBlock.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )

testGlobalVariableInsideFunction = TestCase (
         do code <- readFile "test/testGlobalVariableInsideFunction.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (IntT 1) (evalVar finalState "ans"))
        )