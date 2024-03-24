module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Edit.Internal.EIRules
import Lafont.Edit.Invert
import Lafont.Rewrite.Rules
import TietzeExe.Logging.Primitive

-----------------------------------------------------------------------------------------
-- logWord

word1 :: MonWord
word1 = []

word2 :: MonWord
word2 = [(Symbol "xyz" [])]

word3 :: MonWord
word3 = [(Symbol "xyz" []), (Symbol "qrs" []), (Symbol "qwerty" [])]

word4 :: MonWord
word4 = [(Symbol "xyz" []), (Symbol "qrs" [5]), (Symbol "qwerty" [10, 20])]


test1 = TestCase (assertEqual "logRule can format empty words."
                              "ε"
                              (logWord word1))

test2 = TestCase (assertEqual "logRule can format a one symbol word."
                              "xyz"
                              (logWord word2))

test3 = TestCase (assertEqual "logRule can format a three symbol word."
                              "xyz.qrs.qwerty"
                              (logWord word3))

test4 = TestCase (assertEqual "logRule can format a three symbol word with parameters."
                              "xyz.qrs[5].qwerty[10][20]"
                              (logWord word4))

-----------------------------------------------------------------------------------------
-- logRule

rule1 :: RewriteRule
rule1 = RewriteRule word1 word2 True (Primitive "r1")

rule2 :: RewriteRule
rule2 = RewriteRule word2 word3 True (Primitive "r2")

rule3 :: RewriteRule
rule3 = RewriteRule word1 word2 False (Primitive "r3")

rule4 :: RewriteRule
rule4 = RewriteRule word1 word2 True (Derived $ Just "name")

test5 = TestCase (assertEqual "logRule can display equational rules (1/2)."
                              "rel1: ε = xyz"
                              (logRule ("rel1", rule1)))

test6 = TestCase (assertEqual "logRule can display equational rules (2/2)."
                              "relation2: xyz = xyz.qrs.qwerty"
                              (logRule ("relation2", rule2)))

test7 = TestCase (assertEqual "logRule can display production rules."
                              "rule3: ε → xyz"
                              (logRule ("rule3", rule3)))

test8 = TestCase (assertEqual "logRule can display derived rules."
                              "rel4: ε = xyz"
                              (logRule ("rel4", rule1)))

-----------------------------------------------------------------------------------------
-- logEIRule

Just (sym1, eirule1) = asIRule True "rel1" rule3
Just (sym2, eirule2) = asERule True "name" rule4

test9 = TestCase (assertEqual "logEIRule handles primitive relations."
                              "rel1"
                              (logEIRule eirule1))

test10 = TestCase (assertEqual "logEIRule handles derived relations."
                               "name [derived]"
                               (logEIRule eirule2))

-----------------------------------------------------------------------------------------
-- logEIRuleByQuery

sym3 = Symbol "zzz" []

view0      = createView True
Just view1 = addEIRule view0 (sym1, eirule1)
Just view2 = addEIRule view1 (sym3, eirule2)

test11 = TestCase (assertEqual "logEIRuleByQuery handles failed queries."
                               "xyz: (unknown)"
                               (logEIRuleByQuery view0 sym1))

test12 = TestCase (assertEqual "logEIRuleByQuery handles primitive queries."
                               "xyz: rel1"
                               (logEIRuleByQuery view2 sym1))

test13 = TestCase (assertEqual "logEIRuleByQuery handles primitive queries."
                               "zzz: name [derived]"
                               (logEIRuleByQuery view2 sym3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "logWord_EmptyString" test1,
                                     TestLabel "logWord_OneSymbol" test2,
                                     TestLabel "logWord_ManySymbols" test3,
                                     TestLabel "logWord_Parameters" test4,
                                     TestLabel "logRule_EquationalOne" test5,
                                     TestLabel "logRule_EquationalTwo" test6,
                                     TestLabel "logRule_ProductionRule" test7,
                                     TestLabel "logRule_Derived" test8,
                                     TestLabel "logEIRule_Primitive" test9,
                                     TestLabel "logEIRule_Derived" test10,
                                     TestLabel "logEIRuleByQuery_Fail" test11,
                                     TestLabel "logEIRuleByQuery_Primitive" test12,
                                     TestLabel "logEIRuleByQuery_Derived" test13]

main = defaultMain tests
