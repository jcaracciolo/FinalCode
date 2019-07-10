module CompilerTest(
compilerTests
) where

import Test.HUnit
import Compiler
import DataTypes
import Control.Monad.State.Lazy

compilerTests = TestList [TestLabel "test1" test1]

asd = runStateT (BConst True) [[]]

test1 = TestCase (assertEqual "test1" asd (evalB(BConst True)))