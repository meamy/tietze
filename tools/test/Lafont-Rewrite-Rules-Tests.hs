module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Common
import Lafont.Rewrite.Internal.Rules
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Declares some symbols to ues throughout the test.

x1 :: Symbol
x1 = Symbol "X" [1]

x2 :: Symbol
x2 = Symbol "X" [2]

cx12 :: Symbol
cx12 = Symbol "CX" [1, 2]

cx13 :: Symbol
cx13 = Symbol "CX" [1, 3]

cx23 :: Symbol
cx23 = Symbol "CX" [2, 3]

ccx123 :: Symbol
ccx123 = Symbol "CCX" [1, 2, 3]

z1 :: Symbol
z1 = Symbol "Z" [1]

k12 :: Symbol
k12 = Symbol "K" [1, 2]

-----------------------------------------------------------------------------------------
-- Tests when a rewrite rule is applicable (in either direction).

word1a :: MonWord
word1a = [ccx123, cx13, cx23]

word1b :: MonWord
word1b = [cx13, ccx123, cx23]

rule1 :: RewriteRule
rule1 = RewriteRule [ccx123, cx13] [cx13, ccx123] True Nothing

test1 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 to CCX123.CX13.CX23"
                              (checkRewriteRule word1a rule1 L2R)

test2 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 to CX13.CCX123.CX23"
                              (checkRewriteRule word1b rule1 R2L)

test3 = TestCase $ assertEqual "CCX123.CX13.CX23 =R1=> CX13.CCX123.CX23"
                               word1b (applyRewriteRule word1a rule1 L2R)

test4 = TestCase $ assertEqual "CX13.CCX123.CX23 =R1=> CCX123.CX13.CX23"
                               word1a (applyRewriteRule word1b rule1 R2L)

-----------------------------------------------------------------------------------------
-- Tests when a rewrite rule is not applicable.

rule2 :: RewriteRule
rule2 = RewriteRule [ccx123, cx13, cx23, cx12] [ccx123, cx13, cx23, cx12] True Nothing

rule3 :: RewriteRule
rule3 = RewriteRule [ccx123, cx23] [cx23, ccx123] True Nothing

test5 = TestCase $ assertBool "The rule is too long and must be rejected"
                              (not (checkRewriteRule word1a rule2 L2R))

test6 = TestCase $ assertBool "The rule does not match and must be rejected"
                              (not (checkRewriteRule word1a rule3 L2R))

-----------------------------------------------------------------------------------------
-- Tests when a rewrite operation is applicable (in either direction).

word2a :: MonWord
word2a = [x1, x2, z1, ccx123, cx13, cx23]

word2b :: MonWord
word2b = [x1, x2, z1, cx13, ccx123, cx23]

op1a :: Rewrite
op1a = Rewrite rule1 3 L2R

op1b :: Rewrite
op1b = Rewrite rule1 3 R2L

test7 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 forward at index 3"
                              (checkRewrite word2a op1a)

test8 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 backwards at index 3"
                              (checkRewrite word2b op1b)

test9 = TestCase $ assertEqual "(...).CCX123.CX13.CX23 =(R1@3)=> (...).CX13.CCX123.CX23"
                   word2b (applyRewrite word2a op1a)

test10 = TestCase $ assertEqual "(...).CX13.CCX123.CX23 =(R1@3)=> (...).CCX123.CX13.CX23"
                    word2a (applyRewrite word2b op1b)

word3a :: MonWord
word3a = [k12, x1, x2, z1, ccx123, cx13, cx23]

word3b :: MonWord
word3b = [k12, x1, x2, z1, cx13, ccx123, cx23]

op2a :: Rewrite
op2a = Rewrite rule1 4 L2R

op2b :: Rewrite
op2b = Rewrite rule1 4 R2L

test11 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 forward at index 4"
                               (checkRewrite word3a op2a)

test12 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 backwards at index 4"
                               (checkRewrite word3b op2b)

test13 = TestCase $ assertEqual "(...).CCX123.CX13.CX23 =(R1@4)=> (...).CX13.CCX123.CX23"
                    word3b (applyRewrite word3a op2a)

test14 = TestCase $ assertEqual "(...).CX13.CCX123.CX23 =(R1@4)=> (...).CCX123.CX13.CX23"
                    word3a (applyRewrite word3b op2b)

-----------------------------------------------------------------------------------------
-- Tests edge cases with empty strings.

word4a :: MonWord
word4a = [ccx123, ccx123]

word4b :: MonWord
word4b = []

rule4 :: RewriteRule
rule4 = RewriteRule [ccx123, ccx123] [] True Nothing

test15 = TestCase $ assertEqual "Can support rules that eliminate symbols"
                                word4b (applyRewriteRule word4a rule4 L2R)

test16 = TestCase $ assertEqual "Can support rules that introduce symbols"
                                word4a (applyRewriteRule word4b rule4 R2L)

-----------------------------------------------------------------------------------------
-- Checks that derived rules are identified.

rule5 :: RewriteRule
rule5 = RewriteRule [ccx123, cx13] [cx13, ccx123] True (Just "proof1")

rule6 :: RewriteRule
rule6 = RewriteRule [ccx123, cx13, cx23] [ccx123, cx13, cx23] True (Just "proof2")

test17 = TestCase $ assertBool "Can deteect that a rule is not derived (1/2)"
                               (not (isDerivedRule rule1))

test18 = TestCase $ assertBool "Can deteect that a rule is not derived (2/2)"
                               (not (isDerivedRule rule2))

test19 = TestCase $ assertBool "Can deteect that a rule is derived (1/2)"
                               (isDerivedRule rule5)

test20 = TestCase $ assertBool "Can deteect that a rule is derived (2/2)"
                               (isDerivedRule rule6)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "CheckRuleForwards" test1,
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
                                     TestLabel "ApplyIntroduce" test16,
                                     TestLabel "DetectsNonDerivedRulesOne" test17,
                                     TestLabel "DetectsNonDerivedRulesTwo" test18,
                                     TestLabel "DetectsDerivedRulesOne" test19,
                                     TestLabel "DetectsDerivedRulesTwo" test20]

main = defaultMain tests
