module Main where

import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Declares some gates to ues throughout the test.

x1 :: Gate
x1 = Gate "X" [1]

x2 :: Gate
x2 = Gate "X" [2]

cx12 :: Gate
cx12 = Gate "CX" [1, 2]

cx13 :: Gate
cx13 = Gate "CX" [1, 3]

cx23 :: Gate
cx23 = Gate "CX" [2, 3]

ccx123 :: Gate
ccx123 = Gate "CCX" [1, 2, 3]

z1 :: Gate
z1 = Gate "Z" [1]

k12 :: Gate
k12 = Gate "K" [1, 2]

-----------------------------------------------------------------------------------------
-- Tests when a rewrite rule is applicable (in either direction).

circuit1a :: Circuit
circuit1a = [ccx123, cx13, cx23]

circuit1b :: Circuit
circuit1b = [cx13, ccx123, cx23]

rule1 :: RewriteRule
rule1 = RewriteRule [ccx123, cx13] [cx13, ccx123]

test1 :: Test
test1 = TestCase (assertBool "Can apply CCX123.CX13 = CX13.CCX123 to CCX123.CX13.CX23"
                             (checkRewriteRule circuit1a rule1 True))

test2 :: Test
test2 = TestCase (assertBool "Can apply CCX123.CX13 = CX13.CCX123 to CX13.CCX123.CX23"
                             (checkRewriteRule circuit1b rule1 False))

test3 :: Test
test3 = TestCase (assertEqual "CCX123.CX13.CX23 =R1=> CX13.CCX123.CX23"
                              circuit1b
                              (applyRewriteRule circuit1a rule1 True))

test4 :: Test
test4 = TestCase (assertEqual "CX13.CCX123.CX23 =R1=> CCX123.CX13.CX23"
                              circuit1a
                              (applyRewriteRule circuit1b rule1 False))

-----------------------------------------------------------------------------------------
-- Tests when a rewrite rule is not applicable.

rule2 :: RewriteRule
rule2 = RewriteRule [ccx123, cx13, cx23, cx12] [ccx123, cx13, cx23, cx12]

rule3 :: RewriteRule
rule3 = RewriteRule [ccx123, cx23] [cx23, ccx123]

test5 :: Test
test5 = TestCase (assertBool "The rule is too long and must be rejected"
                             (not (checkRewriteRule circuit1a rule2 True)))

test6 :: Test
test6 = TestCase (assertBool "The rule does not match and must be rejected"
                             (not (checkRewriteRule circuit1a rule3 True)))

-----------------------------------------------------------------------------------------
-- Tests when a rewrite operation is applicable (in either direction).

circuit2a :: Circuit
circuit2a = [x1, x2, z1, ccx123, cx13, cx23]

circuit2b :: Circuit
circuit2b = [x1, x2, z1, cx13, ccx123, cx23]

op1a :: RewriteOp
op1a = RewriteOp rule1 3 True

op1b :: RewriteOp
op1b = RewriteOp rule1 3 False

test7 :: Test
test7 = TestCase (assertBool "Can apply CCX123.CX13 = CX13.CCX123 forward at index 3"
                             (checkRewriteOp circuit2a op1a))

test8 :: Test
test8 = TestCase (assertBool "Can apply CCX123.CX13 = CX13.CCX123 backwards at index 3"
                             (checkRewriteOp circuit2b op1b))

test9 :: Test
test9 = TestCase (assertEqual
                  "X1.X2.Z1.CCX123.CX13.CX23 =(R1 at 3)=> X1.X2.Z1.CX13.CCX123.CX23"
                  circuit2b
                  (applyRewriteOp circuit2a op1a))

test10 :: Test
test10 = TestCase (assertEqual
                   "X1.X2.Z1.CX13.CCX123.CX23 =(R1 at 3)=> X1.X2.Z1.CCX123.CX13.CX23"
                   circuit2a
                   (applyRewriteOp circuit2b op1b))

circuit3a :: Circuit
circuit3a = [k12, x1, x2, z1, ccx123, cx13, cx23]

circuit3b :: Circuit
circuit3b = [k12, x1, x2, z1, cx13, ccx123, cx23]

op2a :: RewriteOp
op2a = RewriteOp rule1 4 True

op2b :: RewriteOp
op2b = RewriteOp rule1 4 False

test11 :: Test
test11 = TestCase (assertBool "Can apply CCX123.CX13 = CX13.CCX123 forward at index 4"
                             (checkRewriteOp circuit3a op2a))

test12 :: Test
test12 = TestCase (assertBool "Can apply CCX123.CX13 = CX13.CCX123 backwards at index 4"
                             (checkRewriteOp circuit3b op2b))

test13 :: Test
test13 = TestCase (assertEqual
                   "K12.X1.X2.Z1.CCX123.CX13.CX23 =(R1 at 4)=> X1.X2.Z1.CX13.CCX123.CX23"
                   circuit3b
                   (applyRewriteOp circuit3a op2a))

test14 :: Test
test14 = TestCase (assertEqual
                   "K12.X1.X2.Z1.CX13.CCX123.CX23 =(R1 at 4)=> X1.X2.Z1.CCX123.CX13.CX23"
                   circuit3a
                   (applyRewriteOp circuit3b op2b))

-----------------------------------------------------------------------------------------
-- Tests edge cases with empty strings.

circuit4a :: Circuit
circuit4a = [ccx123, ccx123]

circuit4b :: Circuit
circuit4b = []

rule4 :: RewriteRule
rule4 = RewriteRule [ccx123, ccx123] []

test15 :: Test
test15 = TestCase (assertEqual "Can support rules that eliminate symbols"
                               circuit4b
                               (applyRewriteRule circuit4a rule4 True))

test16 :: Test
test16 = TestCase (assertEqual "Can support rules that introduce symbols"
                               circuit4a
                               (applyRewriteRule circuit4b rule4 False))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests :: Test
tests = TestList [TestLabel "CheckRuleForwards" test1,
                  TestLabel "CheckRuleBackwards" test2,
                  TestLabel "ApplyRuleForwards" test3,
                  TestLabel "ApplyRuleBackwards" test4,
                  TestLabel "RuleTooLong" test5,
                  TestLabel "RuleDoesNotMatch" test6,
                  TestLabel "CheckOpAt3Forwards" test7,
                  TestLabel "CheckOpAt3Backwards" test8,
                  TestLabel "ApplyOpAt3Forwards" test9,
                  TestLabel "ApplyOpAt3Backwards" test10,
                  TestLabel "CheckOpAt4Forwards" test11,
                  TestLabel "CheckOpAt4Backwards" test12,
                  TestLabel "ApplyOpAt4Forwards" test13,
                  TestLabel "ApplyOpAt4Backwards" test14,
                  TestLabel "ApplyElimination" test15,
                  TestLabel "ApplyIntroduce" test16]

main :: IO Counts
main = runTestTT tests
