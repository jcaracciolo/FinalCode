module CompilerTest(
compilerTests
) where

import Compiler
import Test.HUnit

compilerTests = TestList [TestLabel "test1" test1]



test1 = TestCase (assertEqual "test1" 2 3)