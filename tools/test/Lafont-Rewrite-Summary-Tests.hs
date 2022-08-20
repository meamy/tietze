module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- Building components for summaries (used across tests).

-- Terms in proof summary.

word1 :: MonWord
word1 = [(Symbol "a" []), (Symbol "b" []), (Symbol "c" [])]

word2 :: MonWord
word2 = [(Symbol "x" []), (Symbol "y" [])]

word3 :: MonWord
word3 = []

-- Proof names.

name1 :: String
name1 = "rule1"

name2 :: String
name2 = "tmp"

name3 :: String
name3 = "bad"

meta1 :: RewritePreamble
meta1 = RewritePreamble (Just name1)

meta2 :: RewritePreamble
meta2 = RewritePreamble (Just name2)

meta3 :: RewritePreamble
meta3 = RewritePreamble Nothing

-- Proof summaries.

rule1a :: RewriteRule
rule1a = RewriteRule word1 word2 False (Just name1)

rule1b :: RewriteRule
rule1b = RewriteRule word1 word2 False (Just name2)

rule2 :: RewriteRule
rule2 = RewriteRule word1 word3 False (Just name1)

-----------------------------------------------------------------------------------------
-- createSummaryRule

test1 = TestCase (assertEqual "createSummaryRule can convert a summary into a rule."
                              rule1a
                              (createSummaryRule (DerivationSummary meta1 word1 word2)))

test2 = TestCase (assertEqual "createSummaryRule respects the initial word."
                              (RewriteRule word3 word2 False (Just name1))
                              (createSummaryRule (DerivationSummary meta1 word3 word2)))

test3 = TestCase (assertEqual "createSummaryRule respects the final word."
                              rule2
                              (createSummaryRule (DerivationSummary meta1 word1 word3)))

test4 = TestCase (assertEqual "createSummaryRule respects name in metadata."
                              rule1b
                              (createSummaryRule (DerivationSummary meta2 word1 word2)))

-----------------------------------------------------------------------------------------
-- addSummaryToRules

dict1 = empty `addRule` (name2, rule1b)
dict2 = dict1 `addRule` (name1, rule2)

test5 = TestCase (assertEqual "addSummaryToRules is idempotent for unnamed summaries."
                              (Just dict1 :: Maybe RuleDict)
                              (summary `addSummaryToRules` dict1))
    where summary = DerivationSummary meta3 word1 word2

test6 = TestCase (assertEqual "addSummaryToRules adds rules with new names."
                              (Just dict2 :: Maybe RuleDict)
                              (summary `addSummaryToRules` dict1))
    where summary = DerivationSummary meta1 word1 word3

test7 = TestCase (assertEqual "addSummaryToRules rejects summaries with duplicate names."
                              Nothing
                              (summary `addSummaryToRules` dict1))
    where summary = DerivationSummary meta2 word1 word3

-----------------------------------------------------------------------------------------
-- hasDerivedRule and addDerivedRule

set0 = nullRuleSet
set1 = set0 `addDerivedRule` name1
set2 = set1 `addDerivedRule` name2

test8 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (1/9)"
                             (not (set0 `hasDerivedRule` name1)))

test9 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (2/9)"
                             (not (set0 `hasDerivedRule` name2)))

test10 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (3/9)"
                              (not (set0 `hasDerivedRule` name3)))

test11 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (4/9)"
                              (set1 `hasDerivedRule` name1))

test12 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (5/9)"
                              (not (set1 `hasDerivedRule` name2)))

test13 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (6/9)"
                              (not (set1 `hasDerivedRule` name3)))

test14 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (7/9)"
                              (set2 `hasDerivedRule` name1))

test15 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (8/9)"
                              (set2 `hasDerivedRule` name2))

test16 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (9/9)"
                              (not (set0 `hasDerivedRule` name3)))

-----------------------------------------------------------------------------------------
-- addSummaryToSymbols

set1b = set0 `addDerivedRule` name3
set2b = set1b `addDerivedRule` name1

test17 = TestCase (assertEqual "addSummaryToSymbols is idempotent for unnamed summaries."
                               (Just set2b :: Maybe DRuleSet)
                               (addSummaryToSymbols dict2 set2b summary))
    where summary = DerivationSummary meta3 word1 word2

test18 = TestCase (assertEqual "addSummaryToSymbols adds rules with new names."
                               (Just set2b :: Maybe DRuleSet)
                               (addSummaryToSymbols dict1 set1b summary))
    where summary = DerivationSummary meta1 word1 word3

test19 = TestCase (assertEqual "addSummaryToSymbols rejects summaries with rule names."
                               Nothing
                               (addSummaryToSymbols dict2 set1b summary))
    where summary = DerivationSummary meta2 word1 word3

test20 = TestCase (assertEqual "addSummaryToSymbols rejects summaries with existing names."
                               Nothing
                               (addSummaryToSymbols dict1 set2b summary))
    where summary = DerivationSummary meta2 word1 word3

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "createSummaryRule_Basic" test1,
                                     TestLabel "createSummaryRule_Initial" test2,
                                     TestLabel "createSummaryRule_Final" test3,
                                     TestLabel "createSummaryRule_Meta" test4,
                                     TestLabel "addSummaryToRules_Unnamed" test5,
                                     TestLabel "addSummaryToRules_Success" test6,
                                     TestLabel "addSummaryToRules_Failure" test7,
                                     TestLabel "hasDerivedRule_1" test8,
                                     TestLabel "hasDerivedRule_2" test9,
                                     TestLabel "hasDerivedRule_3" test10,
                                     TestLabel "hasDerivedRule_4" test11,
                                     TestLabel "hasDerivedRule_5" test12,
                                     TestLabel "hasDerivedRule_6" test13,
                                     TestLabel "hasDerivedRule_7" test14,
                                     TestLabel "hasDerivedRule_8" test15,
                                     TestLabel "hasDerivedRule_9" test16,
                                     TestLabel "addSummaryToSymbols_Unnamed" test17,
                                     TestLabel "addSummaryToSymbols_Success" test18,
                                     TestLabel "addSummaryToSymbols_RuleName" test19,
                                     TestLabel "addSummaryToSymbols_Duplicate" test20]

main = defaultMain tests
