module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Simplification

-----------------------------------------------------------------------------------------
-- Declares some symbols to ues throughout the test.

x1 :: Symbol
x1 = Symbol "X" [1]

x2 :: Symbol
x2 = Symbol "X" [2]

x3 :: Symbol
x3 = Symbol "X" [3]

cx12 :: Symbol
cx12 = Symbol "CX" [1, 2]

cx13 :: Symbol
cx13 = Symbol "CX" [1, 3]

ccx123 :: Symbol
ccx123 = Symbol "CCX" [1, 2, 3]

z2 :: Symbol
z2 = Symbol "Z" [2]

k12 :: Symbol
k12 = Symbol "K" [1, 2]

-----------------------------------------------------------------------------------------
-- Edge cases: success.

test1 = TestCase $ assertEqual "Support for empty lists of rewrites on empty words"
                               (RewriteResult [] 0 True) (simplify [] [])

word1 :: MonWord
word1 = [x1, x1]

test2 = TestCase $ assertEqual "Support for empty lists of rewrites"
                               (RewriteResult word1 0 True) (simplify word1 [])

op1 :: Rewrite
op1 = Rewrite (RewriteRule [] [x1, x1] True False) 0 True

test3 = TestCase $ assertEqual "Support for rewrites of an empty words"
                               (RewriteResult word1 1 True) (simplify [] [op1])

-----------------------------------------------------------------------------------------
-- Edge cases: failure.

op2 :: Rewrite
op2 = Rewrite (RewriteRule [x1, x2] [x1, x2] True False) 0 True

test4 = TestCase $ assertEqual "Rejects rewrite when input string is empty"
                               (RewriteResult [] 0 False) (simplify [] [op2])

word2 :: MonWord
word2 = [x1, x1, x1, x1, x2, x2]

oplist1 :: [Rewrite]
oplist1 = [(Rewrite (RewriteRule [] [x1, x1] True False) 0 True),
           (Rewrite (RewriteRule [] [x2, x2] True False) 4 True),
           (Rewrite (RewriteRule [x1, x2] [x1, x2] True False) 3 True),
           (Rewrite (RewriteRule [x1, x2] [x1, x2] True False) 0 True)]

test5 = TestCase $ assertEqual "Reject rewrite after 3 steps"
                               (RewriteResult word2 3 False) (simplify word1 oplist1)

-----------------------------------------------------------------------------------------
-- Non-trivial simplification.

word3a :: MonWord
word3a = [cx12, x3, ccx123, k12, z2, k12, x3]

word3b :: MonWord
word3b = [cx13, cx12, x2, ccx123]

oplist2 :: [Rewrite]
oplist2 = [(Rewrite (RewriteRule [] [x3, x3] True False) 3 True),
           (Rewrite (RewriteRule [ccx123, x3] [x3, ccx123] True False) 2 True),
           (Rewrite (RewriteRule [] [x3, x3] True False) 1 False),
           (Rewrite (RewriteRule [z2, k12] [k12, x2] True False) 4 True),
           (Rewrite (RewriteRule [k12, k12] [] True False) 3 True),
           (Rewrite (RewriteRule [x3, x2] [x2, x3] True False) 2 True),
           (Rewrite (RewriteRule [x3, x3] [] True False) 3 True),
           (Rewrite (RewriteRule [cx13, cx13] [] True False) 2 False),
           (Rewrite (RewriteRule [cx13, x2] [x2, cx13] True False) 3 True),
           (Rewrite (RewriteRule [x2, ccx123] [ccx123, cx13, x2] True False) 1 False),
           (Rewrite (RewriteRule [ccx123, cx13] [cx13, ccx123] True False) 2 True),
           (Rewrite (RewriteRule [x2, cx13] [cx13, x2] True False) 1 True),
           (Rewrite (RewriteRule [cx12, cx13] [cx13, cx12] True False) 0 True)]

test6 = TestCase $ assertEqual "Non-trivial rewrite success"
                               (RewriteResult word3b 13 True) (simplify word3a oplist2)

word4a :: MonWord
word4a = [cx12, x3, ccx123, k12, z2, k12]

word4b :: MonWord
word4b = [cx12, ccx123, x2, x3]

test7 = TestCase $ assertEqual "Non-trivial rewrite success"
                               (RewriteResult word4b 6 False) (simplify word4a oplist2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "AllEmpty" test1,
                                     TestLabel "EmptyRuleList" test2,
                                     TestLabel "EmptyMonWord" test3,
                                     TestLabel "FailAtStep0" test4,
                                     TestLabel "FailAtStep3" test5,
                                     TestLabel "NontrivSuccess" test6,
                                     TestLabel "NontrivFail" test7]

main = defaultMain tests
