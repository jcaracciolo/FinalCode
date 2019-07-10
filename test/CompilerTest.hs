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


compilerTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

asd = runStateT (evalB(BConst True)) [[]]

test1 = TestCase (
            do b <- runStateT (evalB(BConst True)) [[]]
               assertBool "BConst evaluates to True" (fst b)
            )

test2 = TestCase (
         do code <- readFile "test.js"
            case parse (whiteSpace >> program) "" code of
                 Left e  -> print e -- Aca hay que fallar
                 Right r -> do
                            (_, finalState) <- runStateT (eval r) [[]]
                            assertBool "SIZE" ((length (head finalState)) > 0)
        )

