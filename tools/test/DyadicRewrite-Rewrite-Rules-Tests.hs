module Main where

import Test.HUnit
import DyadicRewrite.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Tests when a rewrite rule is applicable (in either direction).

circuit1a :: Circuit
circuit1a = ["CCX123", "CX13", "CX23"]

circuit1b :: Circuit
circuit1b = ["CX13", "CCX123", "CX23"]

rule1 :: RewriteRule
rule1 = RewriteRule ["CCX123", "CX13"] ["CX13", "CCX123"]

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
rule2 = RewriteRule ["CCX123", "CX13", "CX23", "CX12"] ["CCX123", "CX13", "CX23", "CX12"]

rule3 :: RewriteRule
rule3 = RewriteRule ["CX123", "CX23"] ["CX23", "CX123"]

test5 :: Test
test5 = TestCase (assertBool "The rule is too long and must be rejected"
                             (not (checkRewriteRule circuit1a rule2 True)))

test6 :: Test
test6 = TestCase (assertBool "The rule does not match and must be rejected"
                             (not (checkRewriteRule circuit1a rule3 True)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests :: Test
tests = TestList [TestLabel "CheckRuleForwards" test1,
                  TestLabel "CheckRuleBackwards" test2,
                  TestLabel "ApplyRuleForwards" test3,
                  TestLabel "ApplyRuleBackwards" test4,
                  TestLabel "RuleTooLong" test5]

main :: IO Counts
main = runTestTT tests
