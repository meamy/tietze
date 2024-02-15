module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Abstraction
import Lafont.Rewrite.Common
import Lafont.Rewrite.Internal.Summary
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
meta1 = RewritePreamble (Just name1) Nothing

meta2 :: RewritePreamble
meta2 = RewritePreamble (Just name2) Nothing

meta3 :: RewritePreamble
meta3 = RewritePreamble Nothing Nothing

-- Proof summaries.

rule1a :: RewriteRule
rule1a = RewriteRule word1 word2 False (Derived $ Just name1)

rule1b :: RewriteRule
rule1b = RewriteRule word1 word2 False (Derived $ Just name2)

rule1c :: RewriteRule
rule1c = RewriteRule word1 word2 True (Derived $ Just name2)

rule2 :: RewriteRule
rule2 = RewriteRule word1 word3 False (Derived $ Just name1)

rule3 :: RewriteRule
rule3 = RewriteRule word1 word3 True (Primitive "r4")

-- Creates a rule dictionary.

rules1 = addRule empty  ("r1", RewriteRule [] [] False (Primitive "r1"))
rules2 = addRule rules1 ("r2", RewriteRule [] [] False (Primitive "r2"))
rules3 = addRule rules2 ("r3", RewriteRule [] [] False (Primitive "r3"))

-----------------------------------------------------------------------------------------
-- createSummaryRule

summary1 = DerivationSummary meta1 word1 word2
summary2 = DerivationSummary meta1 word3 word2 -- Inital word change.
summary3 = DerivationSummary meta1 word1 word3 -- Final word change.
summary4 = DerivationSummary meta2 word1 word2 -- Name change.
summary5 = DerivationSummary meta3 word1 word2

derForMeta1Neq = AbsDerivation summary1 [Left (Rewrite rule2 0 L2R)]
derForMeta4Eqn = AbsDerivation summary4 [Left (Rewrite rule3 0 L2R)]

dmap0 = makeDerivationMap []
emap0 = identifyEquationalRules dmap0

dmap1 = makeDerivationMap [derForMeta1Neq, derForMeta4Eqn]
emap1 = identifyEquationalRules dmap1

test1 = TestCase (assertEqual "createSummaryRule can convert a summary into a rule."
                              rule1a
                              (createSummaryRule emap0 summary1))

test2 = TestCase (assertEqual "createSummaryRule respects the initial word."
                              (RewriteRule word3 word2 False (Derived $ Just name1))
                              (createSummaryRule emap0 summary2))

test3 = TestCase (assertEqual "createSummaryRule respects the final word."
                              rule2
                              (createSummaryRule emap0 summary3))

test4 = TestCase (assertEqual "createSummaryRule respects name in metadata."
                              rule1b
                              (createSummaryRule emap0 summary4))

test5 = TestCase (assertEqual "createSummaryRule respects emap (1/2)."
                              rule1a
                              (createSummaryRule emap1 summary1))

test6 = TestCase (assertEqual "createSummaryRule respects emap (2/2)."
                              rule1c
                              (createSummaryRule emap1 summary4))

-----------------------------------------------------------------------------------------
-- addSummaryToRules

test7 = TestCase (assertBool "isSummaryEquational handles equational relations."
                             (isSummaryEquational emap1 summary4))

test8 = TestCase (assertBool "isSummaryEquational handles orientated relations."
                             (not (isSummaryEquational emap1 summary1)))

test9 = TestCase (assertBool "isSummaryEquational handles missing entries."
                             (not (isSummaryEquational emap1 summary5)))

-----------------------------------------------------------------------------------------
-- hasDerivedRule and addDerivedRule

set0 = nullRuleSet
set1 = set0 `addDerivedRule` name1
set2 = set1 `addDerivedRule` name2

test10 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (1/9)"
                              (not (set0 `hasDerivedRule` name1)))

test11 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (2/9)"
                              (not (set0 `hasDerivedRule` name2)))

test12 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (3/9)"
                              (not (set0 `hasDerivedRule` name3)))

test13 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (4/9)"
                              (set1 `hasDerivedRule` name1))

test14 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (5/9)"
                              (not (set1 `hasDerivedRule` name2)))

test15 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (6/9)"
                              (not (set1 `hasDerivedRule` name3)))

test16 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (7/9)"
                              (set2 `hasDerivedRule` name1))

test17 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (8/9)"
                              (set2 `hasDerivedRule` name2))

test18 = TestCase (assertBool "hasDerivedRule and addDerivedRule interact (9/9)"
                              (not (set0 `hasDerivedRule` name3)))

-----------------------------------------------------------------------------------------
-- addSummaryToSymbols

dict1 = empty `addRule` (name2, rule1b)
dict2 = dict1 `addRule` (name1, rule2)

set1b = set0 `addDerivedRule` name3
set2b = set1b `addDerivedRule` name1

test19 = TestCase (assertEqual "addSummaryToSymbols is idempotent for unnamed summaries."
                               (Just set2b :: Maybe DRuleSet)
                               (addSummaryToSymbols dict2 set2b summary))
    where summary = DerivationSummary meta3 word1 word2

test20 = TestCase (assertEqual "addSummaryToSymbols adds rules with new names."
                               (Just set2b :: Maybe DRuleSet)
                               (addSummaryToSymbols dict1 set1b summary))
    where summary = DerivationSummary meta1 word1 word3

test21 = TestCase (assertEqual "addSummaryToSymbols rejects summaries with rule names."
                               Nothing
                               (addSummaryToSymbols dict2 set1b summary))
    where summary = DerivationSummary meta2 word1 word3

test22 = TestCase (assertEqual "addSummaryToSymbols rejects summaries with existing names."
                               Nothing
                               (addSummaryToSymbols dict1 set2b summary))
    where summary = DerivationSummary meta2 word1 word3

-----------------------------------------------------------------------------------------
-- addDRule

test23 = TestCase (assertEqual "addDRule handles unnamed derivations."
                               rules3
                               (addDRule rules3 emap1 summary5))

test24 = TestCase (assertEqual "addDRule handles named directed derivations."
                               (addRule rules3 (name1, rule1a))
                               (addDRule rules3 emap1 summary1))

test25 = TestCase (assertEqual "addDRule handles named equational derivations."
                               (addRule rules3 (name2, rule1c))
                               (addDRule rules3 emap1 summary4))

-----------------------------------------------------------------------------------------
-- Respects type property

meta4 :: RewritePreamble
meta4 = RewritePreamble (Just name1) (Just "inverse")

meta5 :: RewritePreamble
meta5 = RewritePreamble (Just name2) (Just "inverse")

test26 = TestCase (assertEqual "The type property is ignored (1/2)."
                               (Just set2b :: Maybe DRuleSet)
                               (addSummaryToSymbols dict1 set1b summary))
    where summary = DerivationSummary meta4 word1 word3

test27 = TestCase (assertEqual "The type property is ignored (2/2)."
                               Nothing
                               (addSummaryToSymbols dict2 set1b summary))
    where summary = DerivationSummary meta5 word1 word3

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "createSummaryRule_Basic" test1,
                                     TestLabel "createSummaryRule_Initial" test2,
                                     TestLabel "createSummaryRule_Final" test3,
                                     TestLabel "createSummaryRule_Meta" test4,
                                     TestLabel "createSummaryRule_Emap_1" test5,
                                     TestLabel "createSummaryRule_Emap_2" test6,
                                     TestLabel "isSummaryEquational_True" test7,
                                     TestLabel "isSummaryEquational_False" test8,
                                     TestLabel "isSummaryEquational_Unnamed" test9,
                                     TestLabel "hasDerivedRule_1" test10,
                                     TestLabel "hasDerivedRule_2" test11,
                                     TestLabel "hasDerivedRule_3" test12,
                                     TestLabel "hasDerivedRule_4" test13,
                                     TestLabel "hasDerivedRule_5" test14,
                                     TestLabel "hasDerivedRule_6" test15,
                                     TestLabel "hasDerivedRule_7" test16,
                                     TestLabel "hasDerivedRule_8" test17,
                                     TestLabel "hasDerivedRule_9" test18,
                                     TestLabel "addSummaryToSymbols_Unnamed" test19,
                                     TestLabel "addSummaryToSymbols_Success" test20,
                                     TestLabel "addSummaryToSymbols_RuleName" test21,
                                     TestLabel "addSummaryToSymbols_Duplicate" test22,
                                     TestLabel "addDRule_unnamed" test23,
                                     TestLabel "addDRule_directed" test24,
                                     TestLabel "addDRule_equational" test25,
                                     TestLabel "type_prop_1" test26,
                                     TestLabel "type_prop_2" test27]

main = defaultMain tests
