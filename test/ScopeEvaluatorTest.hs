module ScopeEvaluatorTest(
scopeEvaluatorTests
) where

import Test.HUnit

scopeEvaluatorTests = TestList [TestLabel "test1" test1]



test1 = TestCase (assertEqual "test1" 5 5)