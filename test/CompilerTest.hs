module CompilerTest(
compilerTests
) where

import Test.HUnit
import Compiler
import DataTypes
import Control.Monad.State.Lazy

compilerTests = TestList [TestLabel "test1" test1]

asd = runStateT (evalB(BConst True)) [[]]

test1 = TestCase (
            do b <- runStateT (evalB(BConst True)) [[]]
               assertBool "BConst evaluates to True" (fst b)
            )

