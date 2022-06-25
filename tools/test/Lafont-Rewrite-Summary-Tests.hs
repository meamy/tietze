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

word1 :: MonWord
word1 = [(Symbol "a" []), (Symbol "b" []), (Symbol "c" [])]

word2 :: MonWord
word2 = [(Symbol "x" []), (Symbol "y" [])]

word3 :: MonWord
word3 = []

meta1 :: RewritePreamble
meta1 = RewritePreamble (Just "rule1")

meta2 :: RewritePreamble
meta2 = RewritePreamble (Just "tmp")

meta3 :: RewritePreamble
meta3 = RewritePreamble Nothing

rule1 :: RewriteRule
rule1 = RewriteRule word1 word2 False True

rule2 :: RewriteRule
rule2 = RewriteRule word1 word3 False True

-----------------------------------------------------------------------------------------
-- createSummaryRule

test1 = TestCase (assertEqual "createSummaryRule can convert a summary into a rule."
                              rule1
                              (createSummaryRule (DerivationSummary meta1 word1 word2)))

test2 = TestCase (assertEqual "createSummaryRule respects the initial word."
                              (RewriteRule word3 word2 False True)
                              (createSummaryRule (DerivationSummary meta1 word3 word2)))

test3 = TestCase (assertEqual "createSummaryRule respects the final word."
                              rule2
                              (createSummaryRule (DerivationSummary meta1 word1 word3)))

test4 = TestCase (assertEqual "createSummaryRule does not depend on metadata."
                              rule1
                              (createSummaryRule (DerivationSummary meta3 word1 word2)))

-----------------------------------------------------------------------------------------
-- addSummaryToRules

dict1 :: RuleDict
dict1 = (empty `addRule` ("tmp", rule1))

dict2 = (dict1 `addRule` ("rule1", rule2))

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
