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

dict1 :: RuleDict
dict1 = (empty `addRule` (name2, rule1b))
dict2 = (dict1 `addRule` (name1, rule2))

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
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "createSummaryRule_Basic" test1,
                                     TestLabel "createSummaryRule_Initial" test2,
                                     TestLabel "createSummaryRule_Final" test3,
                                     TestLabel "createSummaryRule_Meta" test4,
                                     TestLabel "addSummaryToRules_Unnamed" test5,
                                     TestLabel "addSummaryToRules_Success" test6,
                                     TestLabel "addSummaryToRules_Failure" test7]

main = defaultMain tests
