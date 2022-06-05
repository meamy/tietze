module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Lookup

-----------------------------------------------------------------------------------------
-- Relation Dictionaries.

-- This test builds up a relation dictionary and then checks properties. These tests are
-- agnostic to the implementation of RelDict, so will be useful even if the
-- implementation changes.

circ1a :: Circuit
circ1a = [(Gate "a1" []), (Gate "a2" []), (Gate "a3" [])]

circ1b :: Circuit
circ1b = [(Gate "b1" []), (Gate "b2" []), (Gate "b3" [])]

rel1 :: (String, RewriteRule)
rel1 = ("abc", (RewriteRule circ1a circ1b True False))

circ2a :: Circuit
circ2a = [(Gate "c1" []), (Gate "c2" []), (Gate "c3" [])]

circ2b :: Circuit
circ2b = [(Gate "d1" []), (Gate "d2" []), (Gate "d3" [])]

rel2 :: (String, RewriteRule)
rel2 = ("def", (RewriteRule circ2a circ2b True False))

circ3a :: Circuit
circ3a = [(Gate "e1" []), (Gate "e2" []), (Gate "e3" [])]

circ3b :: Circuit
circ3b = [(Gate "f1" []), (Gate "f2" []), (Gate "f3" [])]

rel3 :: (String, RewriteRule)
rel3 = ("ab_12_ef", (RewriteRule circ3a circ3b True False))

sampleDict0 :: RelDict
sampleDict0 = empty
sampleDict1 = addRel sampleDict0 rel1
sampleDict2 = addRel sampleDict1 rel2
sampleDict3 = addRel sampleDict2 rel3

-- Can check if an element is missing.

test1 = TestCase (assertBool "Tests that an empty RelDict does not contain \"abc\"."
                             (not (hasRel sampleDict0 (fst rel1))))

test2 = TestCase (assertBool "Tests that an empty RelDict does not contain \"def\"."
                             (not (hasRel sampleDict0 (fst rel2))))

test3 = TestCase (assertBool "Tests that an empty RelDict does not contain \"ab_12_ef\"."
                             (not (hasRel sampleDict0 (fst rel3))))

test4 = TestCase (assertBool "Tests that an empty RelDict does not contain \"xyz\"."
                             (not (hasRel sampleDict0 "xyz")))

test5 = TestCase (assertBool "Tests that sampleDict1 does not contain \"def\"."
                             (not (hasRel sampleDict1 (fst rel2))))

test6 = TestCase (assertBool "Tests that sampleDict1 does not contain \"ab_12_ef\"."
                             (not (hasRel sampleDict1 (fst rel3))))

test7 = TestCase (assertBool "Tests that sampleDict1 does not contain \"xyz\"."
                             (not (hasRel sampleDict1 "xyz")))

test8 = TestCase (assertBool "Tests that sampleDict2 does not contain \"ab_12_ef\"."
                             (not (hasRel sampleDict2 (fst rel3))))

test9 = TestCase (assertBool "Tests that sampleDict2 does not contain \"xyz\"."
                             (not (hasRel sampleDict2 "xyz")))

test10 = TestCase (assertBool "Tests that sampleDict3 does not contain \"xyz\"."
                              (not (hasRel sampleDict3 "xyz")))

-- Can check if an element is present.

test11 = TestCase (assertBool "Tests that sampleDict1 contains \"abc\"."
                              (hasRel sampleDict1 (fst rel1)))

test12 = TestCase (assertBool "Tests that sampleDict2 contains \"abc\"."
                              (hasRel sampleDict2 (fst rel1)))

test13 = TestCase (assertBool "Tests that sampleDict2 contains \"def\"."
                              (hasRel sampleDict2 (fst rel2)))

test14 = TestCase (assertBool "Tests that sampleDict3 contains \"abc\"."
                              (hasRel sampleDict3 (fst rel1)))

test15 = TestCase (assertBool "Tests that sampleDict3 contains \"def\"."
                              (hasRel sampleDict3 (fst rel2)))

test16 = TestCase (assertBool "Tests that sampleDict3 contains \"ab_12_ef\"."
                              (hasRel sampleDict3 (fst rel3)))

-- Can look up relations.

test17 = TestCase (assertEqual "Tests that sampleDict1 maps \"abc\" to rel1."
                               (Just (snd rel1) :: Maybe RewriteRule)
                               (interpretRel sampleDict1 (fst rel1)))

test18 = TestCase (assertEqual "Tests that sampleDict2 maps \"abc\" to rel1."
                               (Just (snd rel1) :: Maybe RewriteRule)
                               (interpretRel sampleDict2 (fst rel1)))

test19 = TestCase (assertEqual "Tests that sampleDict2 maps \"def\" to rel2."
                               (Just (snd rel2) :: Maybe RewriteRule)
                               (interpretRel sampleDict2 (fst rel2)))

test20 = TestCase (assertEqual "Tests that sampleDict3 maps \"abc\" to rel1."
                               (Just (snd rel1) :: Maybe RewriteRule)
                               (interpretRel sampleDict3 (fst rel1)))

test21 = TestCase (assertEqual "Tests that sampleDict3 maps \"def\" to rel2."
                               (Just (snd rel2) :: Maybe RewriteRule)
                               (interpretRel sampleDict3 (fst rel2)))

test22 = TestCase (assertEqual "Tests that sampleDict3 maps \"ab_12_ef\" to rel3."
                               (Just (snd rel3) :: Maybe RewriteRule)
                               (interpretRel sampleDict3 (fst rel3)))

test23 = TestCase (assertEqual "Tests that sampleDict3 maps \"xyz\" to Nothing."
                               Nothing
                               (interpretRel sampleDict3 "xyz"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "RelDictDoesNotContain_Test0" test1,
                                     TestLabel "RelDictDoesNotContain_Test1" test2,
                                     TestLabel "RelDictDoesNotContain_Test2" test3,
                                     TestLabel "RelDictDoesNotContain_Test3" test4,
                                     TestLabel "RelDictDoesNotContain_Test4" test5,
                                     TestLabel "RelDictDoesNotContain_Test5" test6,
                                     TestLabel "RelDictDoesNotContain_Test6" test7,
                                     TestLabel "RelDictDoesNotContain_Test7" test8,
                                     TestLabel "RelDictDoesNotContain_Test8" test9,
                                     TestLabel "RelDictDoesNotContain_Test9" test10,
                                     TestLabel "RelDictContains_Test0" test11,
                                     TestLabel "RelDictContains_Test1" test12,
                                     TestLabel "RelDictContains_Test2" test13,
                                     TestLabel "RelDictContains_Test3" test14,
                                     TestLabel "RelDictContains_Test4" test15,
                                     TestLabel "RelDictContains_Test5" test16,
                                     TestLabel "RelDictLookup_Test0" test17,
                                     TestLabel "RelDictLookup_Test1" test18,
                                     TestLabel "RelDictLookup_Test2" test19,
                                     TestLabel "RelDictLookup_Test3" test20,
                                     TestLabel "RelDictLookup_Test4" test21,
                                     TestLabel "RelDictLookup_Test5" test22,
                                     TestLabel "RelDictLookup_Test6" test23]

main = defaultMain tests
