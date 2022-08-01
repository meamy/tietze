module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import qualified Lafont.Generators.Semantics as Sem
import Lafont.Generators.RuleSem
import qualified Lafont.Rewrite.Lookup as Rel
import Lafont.Rewrite.Rules

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

ruleA = RewriteRule [symB, symC] [symD] True Nothing                -- [Y] 2*3 = 6
ruleB = RewriteRule [symA, symA, symA] [] True Nothing              -- [Y] 1*1*1 = e
ruleC = RewriteRule [symC, symC] [symE] True Nothing                -- [Y] 3*3 = 9
ruleD = RewriteRule [symA, symD, symA] [symD] True Nothing          -- [Y] 1*6*1 = 6
ruleE = RewriteRule [symB, symB, symC] [symA, symD] True Nothing    -- [N] 2*2*3 = 1*6
ruleF = RewriteRule [symA, symD, symA] [symF] True Nothing          -- [?] 1*6*1 = ??

-----------------------------------------------------------------------------------------
-- Builds dictionaries of generators and relations for the tests.

gens = Sem.empty `Sem.addGen` (genA, Just 1)
                 `Sem.addGen` (genB, Just 2)
                 `Sem.addGen` (genC, Just 3)
                 `Sem.addGen` (genD, Just 6)
                 `Sem.addGen` (genE, Just 9)
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
-- checkRuleSem

test1 = TestCase (assertEqual ""
                              GoodRuleDict
                              (checkRuleSem (*) 1 gens noRules))

test2 = TestCase (assertEqual ""
                              GoodRuleDict
                              (checkRuleSem (*) 1 gens goodRuleDict))

test3 = TestCase (assertEqual ""
                              (InvalidRuleSem ruleEName)
                              (checkRuleSem (*) 1 gens badRuleDict))

test4 = TestCase (assertEqual ""
                              (IncompleteGenSet ruleFName)
                              (checkRuleSem (*) 1 gens missingSemDict))

test5 = TestCase (assertEqual ""
                              (InvalidRuleSem ruleDName)
                              (checkRuleSem (+) 1 gens goodRuleDict))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "checkRuleSem_Empty" test1,
                                     TestLabel "checkRuleSem_Good" test2,
                                     TestLabel "checkRuleSem_Invalid" test3,
                                     TestLabel "checkRuleSem_Incomplete" test4,
                                     TestLabel "checkRuleSem_Compose" test5]

main = defaultMain tests
