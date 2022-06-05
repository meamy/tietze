module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Simplification

-----------------------------------------------------------------------------------------
-- Declares some gates to ues throughout the test.

x1 :: Gate
x1 = Gate "X" [1]

x2 :: Gate
x2 = Gate "X" [2]

x3 :: Gate
x3 = Gate "X" [3]

cx12 :: Gate
cx12 = Gate "CX" [1, 2]

cx13 :: Gate
cx13 = Gate "CX" [1, 3]

ccx123 :: Gate
ccx123 = Gate "CCX" [1, 2, 3]

z2 :: Gate
z2 = Gate "Z" [2]

k12 :: Gate
k12 = Gate "K" [1, 2]

-----------------------------------------------------------------------------------------
-- Edge cases: success.

test1 = TestCase $ assertEqual "Support for empty lists of operations on empty circuits"
                               (RewriteResult [] 0 True) (simplify [] [])

circ1 :: Circuit
circ1 = [x1, x1]

test2 = TestCase $ assertEqual "Support for empty lists of operations"
                               (RewriteResult circ1 0 True) (simplify circ1 [])

op1 :: RewriteOp
op1 = RewriteOp (RewriteRule [] [x1, x1] True False) 0 True

test3 = TestCase $ assertEqual "Support for rewrites of an empty circuits"
                               (RewriteResult circ1 1 True) (simplify [] [op1])

-----------------------------------------------------------------------------------------
-- Edge cases: failure.

op2 :: RewriteOp
op2 = RewriteOp (RewriteRule [x1, x2] [x1, x2] True False) 0 True

test4 = TestCase $ assertEqual "Rejects rewrite when input string is empty"
                               (RewriteResult [] 0 False) (simplify [] [op2])

circ2 :: Circuit
circ2 = [x1, x1, x1, x1, x2, x2]

oplist1 :: [RewriteOp]
oplist1 = [(RewriteOp (RewriteRule [] [x1, x1] True False) 0 True),
           (RewriteOp (RewriteRule [] [x2, x2] True False) 4 True),
           (RewriteOp (RewriteRule [x1, x2] [x1, x2] True False) 3 True),
           (RewriteOp (RewriteRule [x1, x2] [x1, x2] True False) 0 True)]

test5 = TestCase $ assertEqual "Reject rewrite after 3 steps"
                               (RewriteResult circ2 3 False) (simplify circ1 oplist1)

-----------------------------------------------------------------------------------------
-- Non-trivial simplification.

circ3a :: Circuit
circ3a = [cx12, x3, ccx123, k12, z2, k12, x3]

circ3b :: Circuit
circ3b = [cx13, cx12, x2, ccx123]

oplist2 :: [RewriteOp]
oplist2 = [(RewriteOp (RewriteRule [] [x3, x3] True False) 3 True),
           (RewriteOp (RewriteRule [ccx123, x3] [x3, ccx123] True False) 2 True),
           (RewriteOp (RewriteRule [] [x3, x3] True False) 1 False),
           (RewriteOp (RewriteRule [z2, k12] [k12, x2] True False) 4 True),
           (RewriteOp (RewriteRule [k12, k12] [] True False) 3 True),
           (RewriteOp (RewriteRule [x3, x2] [x2, x3] True False) 2 True),
           (RewriteOp (RewriteRule [x3, x3] [] True False) 3 True),
           (RewriteOp (RewriteRule [cx13, cx13] [] True False) 2 False),
           (RewriteOp (RewriteRule [cx13, x2] [x2, cx13] True False) 3 True),
           (RewriteOp (RewriteRule [x2, ccx123] [ccx123, cx13, x2] True False) 1 False),
           (RewriteOp (RewriteRule [ccx123, cx13] [cx13, ccx123] True False) 2 True),
           (RewriteOp (RewriteRule [x2, cx13] [cx13, x2] True False) 1 True),
           (RewriteOp (RewriteRule [cx12, cx13] [cx13, cx12] True False) 0 True)]

test6 = TestCase $ assertEqual "Non-trivial rewrite success"
                               (RewriteResult circ3b 13 True) (simplify circ3a oplist2)

circ4a :: Circuit
circ4a = [cx12, x3, ccx123, k12, z2, k12]

circ4b :: Circuit
circ4b = [cx12, ccx123, x2, x3]

test7 = TestCase $ assertEqual "Non-trivial rewrite success"
                               (RewriteResult circ4b 6 False) (simplify circ4a oplist2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "AllEmpty" test1,
                                     TestLabel "EmptyRuleList" test2,
                                     TestLabel "EmptyCircuit" test3,
                                     TestLabel "FailAtStep0" test4,
                                     TestLabel "FailAtStep3" test5,
                                     TestLabel "NontrivSuccess" test6,
                                     TestLabel "NontrivFail" test7]

main = defaultMain tests
