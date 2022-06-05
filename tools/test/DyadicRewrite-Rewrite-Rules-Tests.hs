module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
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

circ1a :: Circuit
circ1a = [ccx123, cx13, cx23]

circ1b :: Circuit
circ1b = [cx13, ccx123, cx23]

rule1 :: RewriteRule
rule1 = RewriteRule [ccx123, cx13] [cx13, ccx123] True False

test1 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 to CCX123.CX13.CX23"
                              (checkRewriteRule circ1a rule1 True)

test2 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 to CX13.CCX123.CX23"
                              (checkRewriteRule circ1b rule1 False)

test3 = TestCase $ assertEqual "CCX123.CX13.CX23 =R1=> CX13.CCX123.CX23"
                               circ1b (applyRewriteRule circ1a rule1 True)

test4 = TestCase $ assertEqual "CX13.CCX123.CX23 =R1=> CCX123.CX13.CX23"
                               circ1a (applyRewriteRule circ1b rule1 False)

-----------------------------------------------------------------------------------------
-- Tests when a rewrite rule is not applicable.

rule2 :: RewriteRule
rule2 = RewriteRule [ccx123, cx13, cx23, cx12] [ccx123, cx13, cx23, cx12] True False

rule3 :: RewriteRule
rule3 = RewriteRule [ccx123, cx23] [cx23, ccx123] True False

test5 = TestCase $ assertBool "The rule is too long and must be rejected"
                              (not (checkRewriteRule circ1a rule2 True))

test6 = TestCase $ assertBool "The rule does not match and must be rejected"
                              (not (checkRewriteRule circ1a rule3 True))

-----------------------------------------------------------------------------------------
-- Tests when a rewrite operation is applicable (in either direction).

circ2a :: Circuit
circ2a = [x1, x2, z1, ccx123, cx13, cx23]

circ2b :: Circuit
circ2b = [x1, x2, z1, cx13, ccx123, cx23]

op1a :: RewriteOp
op1a = RewriteOp rule1 3 True

op1b :: RewriteOp
op1b = RewriteOp rule1 3 False

test7 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 forward at index 3"
                              (checkRewriteOp circ2a op1a)

test8 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 backwards at index 3"
                              (checkRewriteOp circ2b op1b)

test9 = TestCase $ assertEqual "(...).CCX123.CX13.CX23 =(R1@3)=> (...).CX13.CCX123.CX23"
                   circ2b (applyRewriteOp circ2a op1a)

test10 = TestCase $ assertEqual "(...).CX13.CCX123.CX23 =(R1@3)=> (...).CCX123.CX13.CX23"
                    circ2a (applyRewriteOp circ2b op1b)

circ3a :: Circuit
circ3a = [k12, x1, x2, z1, ccx123, cx13, cx23]

circ3b :: Circuit
circ3b = [k12, x1, x2, z1, cx13, ccx123, cx23]

op2a :: RewriteOp
op2a = RewriteOp rule1 4 True

op2b :: RewriteOp
op2b = RewriteOp rule1 4 False

test11 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 forward at index 4"
                               (checkRewriteOp circ3a op2a)

test12 = TestCase $ assertBool "Can apply CCX123.CX13 = CX13.CCX123 backwards at index 4"
                               (checkRewriteOp circ3b op2b)

test13 = TestCase $ assertEqual "(...).CCX123.CX13.CX23 =(R1@4)=> (...).CX13.CCX123.CX23"
                    circ3b (applyRewriteOp circ3a op2a)

test14 = TestCase $ assertEqual "(...).CX13.CCX123.CX23 =(R1@4)=> (...).CCX123.CX13.CX23"
                    circ3a (applyRewriteOp circ3b op2b)

-----------------------------------------------------------------------------------------
-- Tests edge cases with empty strings.

circ4a :: Circuit
circ4a = [ccx123, ccx123]

circ4b :: Circuit
circ4b = []

rule4 :: RewriteRule
rule4 = RewriteRule [ccx123, ccx123] [] True False

test15 = TestCase $ assertEqual "Can support rules that eliminate symbols"
                                circ4b (applyRewriteRule circ4a rule4 True)

test16 = TestCase $ assertEqual "Can support rules that introduce symbols"
                                circ4a (applyRewriteRule circ4b rule4 False)

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
                                     TestLabel "ApplyIntroduce" test16]

main = defaultMain tests
