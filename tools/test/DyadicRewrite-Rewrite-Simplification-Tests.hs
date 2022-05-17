module Main where

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

test1 :: Test
test1 = TestCase (assertEqual "Support for empty lists of operations on empty circuits"
                              (RewriteResult [] 0 True)
                              (simplify [] []))

circuit1 :: Circuit
circuit1 = [x1, x1]

test2 :: Test
test2 = TestCase (assertEqual "Support for empty lists of operations"
                              (RewriteResult circuit1 0 True)
                              (simplify circuit1 []))

op1 :: RewriteOp
op1 = RewriteOp (RewriteRule [] [x1, x1]) 0 True

test3 :: Test
test3 = TestCase (assertEqual "Support for rewrites of an empty circuits"
                              (RewriteResult circuit1 1 True)
                              (simplify [] [op1]))

-----------------------------------------------------------------------------------------
-- Edge cases: failure.

op2 :: RewriteOp
op2 = RewriteOp (RewriteRule [x1, x2] [x1, x2]) 0 True

test4 :: Test
test4 = TestCase (assertEqual "Rejects rewrite when input string is empty"
                              (RewriteResult [] 0 False)
                              (simplify [] [op2]))

circuit2 :: Circuit
circuit2 = [x1, x1, x1, x1, x2, x2]

op3 :: RewriteOp
op3 = RewriteOp (RewriteRule [] [x2, x2]) 4 True

op4 :: RewriteOp
op4 = RewriteOp (RewriteRule [x1, x2] [x1, x2]) 3 True

test5 :: Test
test5 = TestCase (assertEqual "Reject rewrite after 3 steps"
                              (RewriteResult circuit2 3 False)
                              (simplify circuit1 [op1, op3, op4, op2]))

-----------------------------------------------------------------------------------------
-- Non-trivial simplification.

circuit3a :: Circuit
circuit3a = [cx12, x3, ccx123, k12, z2, k12, x3]

circuit3b :: Circuit
circuit3b = [cx13, cx12, x2, ccx123]

oplist :: [RewriteOp]
oplist = [(RewriteOp (RewriteRule [] [x3, x3]) 3 True),
          (RewriteOp (RewriteRule [ccx123, x3] [x3, ccx123]) 2 True),
          (RewriteOp (RewriteRule [] [x3, x3]) 1 False),
          (RewriteOp (RewriteRule [z2, k12] [k12, x2]) 4 True),
          (RewriteOp (RewriteRule [k12, k12] []) 3 True),
          (RewriteOp (RewriteRule [x3, x2] [x2, x3]) 2 True),
          (RewriteOp (RewriteRule [x3, x3] []) 3 True),
          (RewriteOp (RewriteRule [cx13, cx13] []) 2 False),
          (RewriteOp (RewriteRule [cx13, x2] [x2, cx13]) 3 True),
          (RewriteOp (RewriteRule [x2, ccx123] [ccx123, cx13, x2]) 1 False),
          (RewriteOp (RewriteRule [ccx123, cx13] [cx13, ccx123]) 2 True),
          (RewriteOp (RewriteRule [x2, cx13] [cx13, x2]) 1 True),
          (RewriteOp (RewriteRule [cx12, cx13] [cx13, cx12]) 0 True)]

test6 :: Test
test6 = TestCase (assertEqual "Non-trivial rewrite success"
                              (RewriteResult circuit3b 13 True)
                              (simplify circuit3a oplist))

circuit4a :: Circuit
circuit4a = [cx12, x3, ccx123, k12, z2, k12]

circuit4b :: Circuit
circuit4b = [cx12, ccx123, x2, x3]

test7 :: Test
test7 = TestCase (assertEqual "Non-trivial rewrite success"
                              (RewriteResult circuit4b 6 False)
                              (simplify circuit4a oplist))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests :: Test
tests = TestList [TestLabel "AllEmpty" test1,
                  TestLabel "EmptyRuleList" test2,
                  TestLabel "EmptyCircuit" test3,
                  TestLabel "FailAtStep0" test4,
                  TestLabel "FailAtStep3" test5,
                  TestLabel "NontrivSuccess" test6,
                  TestLabel "NontrivFail" test7]

main :: IO Counts
main = runTestTT tests
