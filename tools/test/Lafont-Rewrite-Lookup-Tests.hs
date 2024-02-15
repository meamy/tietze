module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Lookup

-----------------------------------------------------------------------------------------
-- Rule Dictionaries.

-- This test builds up a rule dictionary and then checks properties. These tests are
-- agnostic to the implementation of RuleDict, so will be useful even if the
-- implementation changes.

word1a :: MonWord
word1a = [(Symbol "a1" []), (Symbol "a2" []), (Symbol "a3" [])]

word1b :: MonWord
word1b = [(Symbol "b1" []), (Symbol "b2" []), (Symbol "b3" [])]

rel1 :: (String, RewriteRule)
rel1 = ("abc", (RewriteRule word1a word1b True (Primitive "abc")))

word2a :: MonWord
word2a = [(Symbol "c1" []), (Symbol "c2" []), (Symbol "c3" [])]

word2b :: MonWord
word2b = [(Symbol "d1" []), (Symbol "d2" []), (Symbol "d3" [])]

rel2 :: (String, RewriteRule)
rel2 = ("def", (RewriteRule word2a word2b True (Primitive "def")))

word3a :: MonWord
word3a = [(Symbol "e1" []), (Symbol "e2" []), (Symbol "e3" [])]

word3b :: MonWord
word3b = [(Symbol "f1" []), (Symbol "f2" []), (Symbol "f3" [])]

rel3 :: (String, RewriteRule)
rel3 = ("ab_12_ef", (RewriteRule word3a word3b True (Primitive "ab_12_ef")))

sampleDict0 :: RuleDict
sampleDict0 = empty
sampleDict1 = addRule sampleDict0 rel1
sampleDict2 = addRule sampleDict1 rel2
sampleDict3 = addRule sampleDict2 rel3

-- Can check if an element is missing.

test1 = TestCase (assertBool "Tests that an empty RuleDict does not contain \"abc\"."
                             (not (hasRule sampleDict0 (fst rel1))))

test2 = TestCase (assertBool "Tests that an empty RuleDict does not contain \"def\"."
                             (not (hasRule sampleDict0 (fst rel2))))

test3 = TestCase (assertBool "Tests that an empty RuleDict does not contain \"ab_12_ef\"."
                             (not (hasRule sampleDict0 (fst rel3))))

test4 = TestCase (assertBool "Tests that an empty RuleDict does not contain \"xyz\"."
                             (not (hasRule sampleDict0 "xyz")))

test5 = TestCase (assertBool "Tests that sampleDict1 does not contain \"def\"."
                             (not (hasRule sampleDict1 (fst rel2))))

test6 = TestCase (assertBool "Tests that sampleDict1 does not contain \"ab_12_ef\"."
                             (not (hasRule sampleDict1 (fst rel3))))

test7 = TestCase (assertBool "Tests that sampleDict1 does not contain \"xyz\"."
                             (not (hasRule sampleDict1 "xyz")))

test8 = TestCase (assertBool "Tests that sampleDict2 does not contain \"ab_12_ef\"."
                             (not (hasRule sampleDict2 (fst rel3))))

test9 = TestCase (assertBool "Tests that sampleDict2 does not contain \"xyz\"."
                             (not (hasRule sampleDict2 "xyz")))

test10 = TestCase (assertBool "Tests that sampleDict3 does not contain \"xyz\"."
                              (not (hasRule sampleDict3 "xyz")))

-- Can check if an element is present.

test11 = TestCase (assertBool "Tests that sampleDict1 contains \"abc\"."
                              (hasRule sampleDict1 (fst rel1)))

test12 = TestCase (assertBool "Tests that sampleDict2 contains \"abc\"."
                              (hasRule sampleDict2 (fst rel1)))

test13 = TestCase (assertBool "Tests that sampleDict2 contains \"def\"."
                              (hasRule sampleDict2 (fst rel2)))

test14 = TestCase (assertBool "Tests that sampleDict3 contains \"abc\"."
                              (hasRule sampleDict3 (fst rel1)))

test15 = TestCase (assertBool "Tests that sampleDict3 contains \"def\"."
                              (hasRule sampleDict3 (fst rel2)))

test16 = TestCase (assertBool "Tests that sampleDict3 contains \"ab_12_ef\"."
                              (hasRule sampleDict3 (fst rel3)))

-- Can look up rules.

test17 = TestCase (assertEqual "Tests that sampleDict1 maps \"abc\" to rel1."
                               (Just (snd rel1) :: Maybe RewriteRule)
                               (interpretRule sampleDict1 (fst rel1)))

test18 = TestCase (assertEqual "Tests that sampleDict2 maps \"abc\" to rel1."
                               (Just (snd rel1) :: Maybe RewriteRule)
                               (interpretRule sampleDict2 (fst rel1)))

test19 = TestCase (assertEqual "Tests that sampleDict2 maps \"def\" to rel2."
                               (Just (snd rel2) :: Maybe RewriteRule)
                               (interpretRule sampleDict2 (fst rel2)))

test20 = TestCase (assertEqual "Tests that sampleDict3 maps \"abc\" to rel1."
                               (Just (snd rel1) :: Maybe RewriteRule)
                               (interpretRule sampleDict3 (fst rel1)))

test21 = TestCase (assertEqual "Tests that sampleDict3 maps \"def\" to rel2."
                               (Just (snd rel2) :: Maybe RewriteRule)
                               (interpretRule sampleDict3 (fst rel2)))

test22 = TestCase (assertEqual "Tests that sampleDict3 maps \"ab_12_ef\" to rel3."
                               (Just (snd rel3) :: Maybe RewriteRule)
                               (interpretRule sampleDict3 (fst rel3)))

test23 = TestCase (assertEqual "Tests that sampleDict3 maps \"xyz\" to Nothing."
                               Nothing
                               (interpretRule sampleDict3 "xyz"))

-- Can fold over relations.

accumulator1 :: (String, RewriteRule) -> [String] -> [String]
accumulator1 (_, rule) acc = (name (head (lhs rule))):acc

test24 = TestCase (assertEqual "Folding accumulator1 on sampleDict0 is successful."
                               [""]
                               (foldRules accumulator1 [""] sampleDict0))

test25 = TestCase (assertEqual "Folding accumulator1 on sampleDict1 is successful."
                               ["a1", ""]
                               (foldRules accumulator1 [""] sampleDict1))

test26 = TestCase (assertEqual "Folding accumulator1 on sampleDict2 is successful."
                               ["a1", "c1", ""]
                               (foldRules accumulator1 [""] sampleDict2))

test27 = TestCase (assertEqual "Folding accumulator1 on sampleDict3 is successful."
                               ["e1", "a1", "c1", ""]
                               (foldRules accumulator1 [""] sampleDict3))

accumulator2 :: (String, RewriteRule) -> [String] -> [String]
accumulator2 (_, rule) acc = ((name (head (rhs rule)))):acc

test28 = TestCase (assertEqual "Folding accumulator1 on sampleDict0 is successful."
                               [""]
                               (foldRules accumulator2 [""] sampleDict0))

test29 = TestCase (assertEqual "Folding accumulator1 on sampleDict1 is successful."
                               ["b1", ""]
                               (foldRules accumulator2 [""] sampleDict1))

test30 = TestCase (assertEqual "Folding accumulator1 on sampleDict2 is successful."
                               ["b1", "d1", ""]
                               (foldRules accumulator2 [""] sampleDict2))

test31 = TestCase (assertEqual "Folding accumulator1 on sampleDict3 is successful."
                               ["f1", "b1", "d1", ""]
                               (foldRules accumulator2 [""] sampleDict3))


-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "RuleDictDoesNotContain_Test0" test1,
                                     TestLabel "RuleDictDoesNotContain_Test1" test2,
                                     TestLabel "RuleDictDoesNotContain_Test2" test3,
                                     TestLabel "RuleDictDoesNotContain_Test3" test4,
                                     TestLabel "RuleDictDoesNotContain_Test4" test5,
                                     TestLabel "RuleDictDoesNotContain_Test5" test6,
                                     TestLabel "RuleDictDoesNotContain_Test6" test7,
                                     TestLabel "RuleDictDoesNotContain_Test7" test8,
                                     TestLabel "RuleDictDoesNotContain_Test8" test9,
                                     TestLabel "RuleDictDoesNotContain_Test9" test10,
                                     TestLabel "RuleDictContains_Test0" test11,
                                     TestLabel "RuleDictContains_Test1" test12,
                                     TestLabel "RuleDictContains_Test2" test13,
                                     TestLabel "RuleDictContains_Test3" test14,
                                     TestLabel "RuleDictContains_Test4" test15,
                                     TestLabel "RuleDictContains_Test5" test16,
                                     TestLabel "RuleDictLookup_Test0" test17,
                                     TestLabel "RuleDictLookup_Test1" test18,
                                     TestLabel "RuleDictLookup_Test2" test19,
                                     TestLabel "RuleDictLookup_Test3" test20,
                                     TestLabel "RuleDictLookup_Test4" test21,
                                     TestLabel "RuleDictLookup_Test5" test22,
                                     TestLabel "RuleDictLookup_Test6" test23,
                                     TestLabel "RuleFolding_Test0" test24,
                                     TestLabel "RuleFolding_Test1" test25,
                                     TestLabel "RuleFolding_Test2" test26,
                                     TestLabel "RuleFolding_Test3" test27,
                                     TestLabel "RuleFolding_Test4" test28,
                                     TestLabel "RuleFolding_Test5" test29,
                                     TestLabel "RuleFolding_Test6" test30,
                                     TestLabel "RuleFolding_Test7" test31]

main = defaultMain tests
