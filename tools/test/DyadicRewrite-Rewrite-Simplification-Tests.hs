module Main where

import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Simplification

-----------------------------------------------------------------------------------------
-- Edge cases: success.

test1 :: Test
test1 = TestCase (assertEqual "Support for empty lists of operations on empty circuits"
                              (RewriteResult [] 0 True)
                              (simplify [] []))

circuit1 :: Circuit
circuit1 = ["X1", "X1"]

test2 :: Test
test2 = TestCase (assertEqual "Support for empty lists of operations"
                              (RewriteResult circuit1 0 True)
                              (simplify circuit1 []))

op1 :: RewriteOp
op1 = RewriteOp (RewriteRule [] ["X1", "X1"]) 0 True

test3 :: Test
test3 = TestCase (assertEqual "Support for rewrites of an empty circuits"
                              (RewriteResult circuit1 1 True)
                              (simplify [] [op1]))

-----------------------------------------------------------------------------------------
-- Edge cases: failure.

op2 :: RewriteOp
op2 = RewriteOp (RewriteRule ["X1", "X2"] ["X1", "X2"]) 0 True

test4 :: Test
test4 = TestCase (assertEqual "Rejects rewrite when input string is empty"
                              (RewriteResult [] 0 False)
                              (simplify [] [op2]))

circuit2 :: Circuit
circuit2 = ["X1", "X1", "X1", "X1", "X2", "X2"]

op3 :: RewriteOp
op3 = RewriteOp (RewriteRule [] ["X2", "X2"]) 4 True

op4 :: RewriteOp
op4 = RewriteOp (RewriteRule ["X1", "X2"] ["X1", "X2"]) 3 True

test5 :: Test
test5 = TestCase (assertEqual "Reject rewrite after 3 steps"
                              (RewriteResult circuit2 3 False)
                              (simplify circuit1 [op1, op3, op4, op2]))

-----------------------------------------------------------------------------------------
-- Non-trivial simplification.

circuit3a :: Circuit
circuit3a = ["CX12", "X3", "CCX123", "K12", "Z2", "K12", "X3"]

circuit3b :: Circuit
circuit3b = ["CX13", "CX12", "X2", "CCX123"]

oplist :: [RewriteOp]
oplist = [(RewriteOp (RewriteRule [] ["X3", "X3"]) 3 True),
          (RewriteOp (RewriteRule ["CCX123", "X3"] ["X3", "CCX123"]) 2 True),
          (RewriteOp (RewriteRule [] ["X3", "X3"]) 1 False),
          (RewriteOp (RewriteRule ["Z2", "K12"] ["K12", "X2"]) 4 True),
          (RewriteOp (RewriteRule ["K12", "K12"] []) 3 True),
          (RewriteOp (RewriteRule ["X3", "X2"] ["X2", "X3"]) 2 True),
          (RewriteOp (RewriteRule ["X3", "X3"] []) 3 True),
          (RewriteOp (RewriteRule ["CX13", "CX13"] []) 2 False),
          (RewriteOp (RewriteRule ["CX13", "X2"] ["X2", "CX13"]) 3 True),
          (RewriteOp (RewriteRule ["X2", "CCX123"] ["CCX123", "CX13", "X2"]) 1 False),
          (RewriteOp (RewriteRule ["CCX123", "CX13"] ["CX13", "CCX123"]) 2 True),
          (RewriteOp (RewriteRule ["X2", "CX13"] ["CX13", "X2"]) 1 True),
          (RewriteOp (RewriteRule ["CX12", "CX13"] ["CX13", "CX12"]) 0 True)]

test6 :: Test
test6 = TestCase (assertEqual "Non-trivial rewrite success"
                              (RewriteResult circuit3b 13 True)
                              (simplify circuit3a oplist))

circuit4a :: Circuit
circuit4a = ["CX12", "X3", "CCX123", "K12", "Z2", "K12"]

circuit4b :: Circuit
circuit4b = ["CX12", "CCX123", "X2", "X3"]

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
