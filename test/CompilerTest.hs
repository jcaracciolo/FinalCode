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
import Control.Exception

compilerTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

asd = runStateT (evalB(BConst True)) [[]]

test1 = TestCase (
            do b <- runStateT (evalB(BConst True)) [[]]
               assertBool "BConst evaluates to True" (fst b)
            )

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

assertError::SomeException -> Assertion
assertError _ = assertBool "Exception was thrown" True
