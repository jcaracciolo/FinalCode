module IntegrationTest(
integrationTests
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


integrationTests = TestList [
                    TestLabel "IF/ELSE statement" testIfElse,
                    TestLabel "WHILE statement" testWhile,
                    TestLabel "Bool && Arithmetic" testBoolAndArithmetic,
                    TestLabel "Let variable in scope" testLetVariableInScope,
                    TestLabel "Let variable not in scope" testLetVariableNotInScope,
                    TestLabel "Var variable in scope" testVarVariableInScope,
                    TestLabel "Var variable not in scope" testVarVariableNotInScope,
                    TestLabel "Global variable visible from block" testGlobalVariableInsideBlock,
                    TestLabel "Global variable visible from function" testGlobalVariableInsideFunction,
                    TestLabel "Function returns value" testFunctionReturnsValue,
                    TestLabel "Code is not executed below return" testNoExecutionAfterReturn,
                    TestLabel "Function called with 1 parameter" testCallFunctionOneParameter,
                    TestLabel "Function called with 2 parameters" testCallFunctionTwoParameters,
                    TestLabel "Function called with less parameters than required" testCallFunctionLessParameters,
                    TestLabel "Function called with more parameters than required" testCallFunctionMoreParameters,
                    TestLabel "Call function returned by another function" testCallFunctionReturnedByFunction,
                    TestLabel "Define Object" testObject,
                    TestLabel "Access object property" testObjectProperty,
                    TestLabel "Access and call object function" testObjectFunction,
                    TestLabel "Use object property inside if" testObjectPropertyInsideIf,
                    TestLabel "Object inside if fails" testObjectInsideIfFails
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
         do code <- readFile "test/resources/testIfElse.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testWhile = TestCase (
         do code <- readFile "test/resources/testWhile.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 10) (evalVar finalState "ans"))
        )


testBoolAndArithmetic = TestCase (
         do code <- readFile "test/resources/testBoolAndArithmetic.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            catch (assertEqual "ans equals 2" (NumericT 2) (evalVar finalState "ans")) assertError
         )



testLetVariableInScope = TestCase (
         do code <- readFile "test/resources/testLetVariableInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )

testLetVariableNotInScope = TestCase (
         do code <- readFile "test/resources/testLetVariableNotInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            catch (assertEqual "ans equals 12" (NumericT 12) (evalVar finalState "ans")) assertError
         )


testVarVariableInScope = TestCase (
         do code <- readFile "test/resources/testVarVariableInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )

testVarVariableNotInScope = TestCase (
         do code <- readFile "test/resources/testVarVariableNotInScope.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            catch (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans")) assertError
         )


testGlobalVariableInsideBlock = TestCase (
         do code <- readFile "test/resources/testGlobalVariableInsideBlock.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testGlobalVariableInsideFunction = TestCase (
         do code <- readFile "test/resources/testGlobalVariableInsideFunction.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testFunctionReturnsValue = TestCase (
         do code <- readFile "test/resources/testFunctionReturnsValue.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testNoExecutionAfterReturn = TestCase (
         do code <- readFile "test/resources/testNoExecutionAfterReturn.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testCallFunctionOneParameter = TestCase (
         do code <- readFile "test/resources/testCallFunctionOneParameter.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testCallFunctionTwoParameters = TestCase (
         do code <- readFile "test/resources/testCallFunctionTwoParameters.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testCallFunctionLessParameters = TestCase (
         do code <- readFile "test/resources/testCallFunctionLessParameters.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            catch (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans")) assertError
         )


testCallFunctionMoreParameters = TestCase (
         do code <- readFile "test/resources/testCallFunctionMoreParameters.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )

testCallFunctionReturnedByFunction = TestCase (
         do code <- readFile "test/resources/testCallFunctionReturnedByFunction.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testObject = TestCase (
         do code <- readFile "test/resources/testObject.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testObjectProperty = TestCase (
         do code <- readFile "test/resources/testObjectProperty.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testObjectFunction = TestCase (
         do code <- readFile "test/resources/testObjectFunction.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testObjectPropertyInsideIf = TestCase (
         do code <- readFile "test/resources/testObjectPropertyInsideIf.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans"))
        )


testObjectInsideIfFails = TestCase (
         do code <- readFile "test/resources/testCallFunctionLessParameters.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> (assertFailure "Parse Failed")
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            catch (assertEqual "ans equals 1" (NumericT 1) (evalVar finalState "ans")) assertError
         )