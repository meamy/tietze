module Main where

import qualified Tietze.Generators.Semantics as Sem
import qualified Tietze.Rewrite.Lookup as Rel

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Tietze.Common
import Tietze.Generators.Categories
import Tietze.Generators.RuleSem
import Tietze.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Defines generators and rules.

genA = "a"
genB = "b"
genC = "c"
genD = "d"
genE = "e"
genF = "f"

symA = Symbol genA []
symB = Symbol genB []
symC = Symbol genC []
symD = Symbol genD []
symE = Symbol genE []
symF = Symbol genF []

ruleAName = "rule1"
ruleBName = "rule2"
ruleCName = "rule3"
ruleDName = "rule4"
ruleEName = "rule5"
ruleFName = "rule6"

ruleA = RewriteRule [symB, symC] [symD] True (Primitive "r1")             -- [Y] 2*3 = 6
ruleB = RewriteRule [symA, symA, symA] [] True (Primitive "r2")           -- [Y] 1*1*1 = e
ruleC = RewriteRule [symC, symC] [symE] True (Primitive "r3")             -- [Y] 3*3 = 9
ruleD = RewriteRule [symA, symD, symA] [symD] True (Primitive "r4")       -- [Y] 1*6*1 = 6
ruleE = RewriteRule [symB, symB, symC] [symA, symD] True (Primitive "r5") -- [N] 2*2*3 = 1*6
ruleF = RewriteRule [symA, symD, symA] [symF] True (Primitive "r6")       -- [?] 1*6*1 = ??

-----------------------------------------------------------------------------------------
-- Builds dictionaries of generators and relations for multiplicative tests.

multIntGens = Sem.empty `Sem.addGen` (genA, Just (MultInt 1))
                        `Sem.addGen` (genB, Just (MultInt 2))
                        `Sem.addGen` (genC, Just (MultInt 3))
                        `Sem.addGen` (genD, Just (MultInt 6))
                        `Sem.addGen` (genE, Just (MultInt 9))
                        `Sem.addGen` (genF, Nothing)

noRules = Rel.empty

goodRuleDict = Rel.empty `Rel.addRule` (ruleAName, ruleA)
                         `Rel.addRule` (ruleBName, ruleB)
                         `Rel.addRule` (ruleCName, ruleC)
                         `Rel.addRule` (ruleDName, ruleD)

badRuleDict = Rel.empty `Rel.addRule` (ruleAName, ruleA)
                        `Rel.addRule` (ruleBName, ruleB)
                        `Rel.addRule` (ruleEName, ruleE)
                        `Rel.addRule` (ruleDName, ruleD)

missingSemDict = Rel.empty `Rel.addRule` (ruleAName, ruleA)
                           `Rel.addRule` (ruleBName, ruleB)
                           `Rel.addRule` (ruleFName, ruleF)
                           `Rel.addRule` (ruleDName, ruleD)

-----------------------------------------------------------------------------------------
-- Builds dictionaries of generators and relations for additive tests.

addIntGens = Sem.empty `Sem.addGen` (genA, Just (AddInt 0))
                       `Sem.addGen` (genB, Just (AddInt 2))
                       `Sem.addGen` (genC, Just (AddInt 3))
                       `Sem.addGen` (genD, Just (AddInt 5))
                       `Sem.addGen` (genE, Just (AddInt 6))
                       `Sem.addGen` (genF, Nothing)

goodAdditiveRule = Rel.empty `Rel.addRule` (ruleAName, ruleA)
                             `Rel.addRule` (ruleBName, ruleB)
                             `Rel.addRule` (ruleFName, ruleC)
                             `Rel.addRule` (ruleDName, ruleD)

badAdditiveRule = Rel.empty `Rel.addRule` (ruleAName, ruleE)

-----------------------------------------------------------------------------------------
-- checkRuleSem

test1 = TestCase (assertEqual "Tests that an empty list of rules is valid."
                              GoodRuleDict
                              (checkRuleSem multIntGens noRules))

test2 = TestCase (assertEqual "Tests that a list of valid rules is accepted"
                              GoodRuleDict
                              (checkRuleSem multIntGens goodRuleDict))

test3 = TestCase (assertEqual "Tests that a bad status is returned for an invalid rule."
                              (InvalidRuleSem ruleEName)
                              (checkRuleSem multIntGens badRuleDict))

test4 = TestCase (assertEqual "Tests that a bad status is returned for unknown gens."
                              (IncompleteGenSet ruleFName)
                              (checkRuleSem multIntGens missingSemDict))

test5 = TestCase (assertEqual "Positive status for alternative semantics (+, not *)."
                              GoodRuleDict
                              (checkRuleSem addIntGens goodAdditiveRule))

test6 = TestCase (assertEqual "Negative status for alternative semantics (+, not *)."
                              (InvalidRuleSem ruleAName)
                              (checkRuleSem addIntGens badAdditiveRule))

-----------------------------------------------------------------------------------------
-- isRuleDictStatusGood

test7 = TestCase (assertBool "isRuleDictStatusGood (1/3)."
                             (isRuleDictStatusGood GoodRuleDict))

test8 = TestCase (assertBool "isRuleDictStatusGood (2/3)."
                             (not (isRuleDictStatusGood (InvalidRuleSem "relname"))))

test9 = TestCase (assertBool "isRuleDictStatusGood (3/3)."
                             (not (isRuleDictStatusGood (IncompleteGenSet "relname"))))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "checkRuleSem_Empty" test1,
                                     TestLabel "checkRuleSem_Good" test2,
                                     TestLabel "checkRuleSem_Invalid" test3,
                                     TestLabel "checkRuleSem_Incomplete" test4,
                                     TestLabel "checkRuleSem_AltSem_Good" test5,
                                     TestLabel "checkRuleSem_AltSem_Invalid" test6,
                                     TestLabel "isRuleDictStatusGood_Good" test7,
                                     TestLabel "isRuleDictStatusGood_Invalid" test8,
                                     TestLabel "isRuleDictStatusGood_Incomplete" test9]

main = defaultMain tests
