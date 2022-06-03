module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Generators.Semantics

-----------------------------------------------------------------------------------------
-- Generator Dictionaries.

-- This test builds up a generator dictionary and then checks properties. These tests are
-- agnostic to the implementation of GenDict, so will be useful even if the
-- the implementation changes.

gen1 :: (String, Maybe Int)
gen1 = ("abc", Just 1)

gen2 :: (String, Maybe Int)
gen2 = ("def", Just 2)

gen3 :: (String, Maybe Int)
gen3 = ("ab_12_ef", Nothing)

sampleDict0 :: GenDict Int
sampleDict0 = empty
sampleDict1 = addGen sampleDict0 gen1
sampleDict2 = addGen sampleDict1 gen2
sampleDict3 = addGen sampleDict2 gen3

-- Can check if an element is missing.

test1 = TestCase (assertBool "Tests that an empty GenDict does not contain \"abc\"."
                             (not (hasGen sampleDict0 (fst gen1))))

test2 = TestCase (assertBool "Tests that an empty GenDict does not contain \"def\"."
                             (not (hasGen sampleDict0 (fst gen2))))

test3 = TestCase (assertBool "Tests that an empty GenDict does not contain \"ab_12_ef\"."
                             (not (hasGen sampleDict0 (fst gen3))))

test4 = TestCase (assertBool "Tests that an empty GenDict does not contain \"xyz\"."
                             (not (hasGen sampleDict0 "xyz")))

test5 = TestCase (assertBool "Tests that sampleDict1 does not contain \"def\"."
                             (not (hasGen sampleDict1 (fst gen2))))

test6 = TestCase (assertBool "Tests that sampleDict1 does not contain \"ab_12_ef\"."
                             (not (hasGen sampleDict1 (fst gen3))))

test7 = TestCase (assertBool "Tests that sampleDict1 does not contain \"xyz\"."
                             (not (hasGen sampleDict1 "xyz")))

test8 = TestCase (assertBool "Tests that sampleDict2 does not contain \"ab_12_ef\"."
                             (not (hasGen sampleDict2 (fst gen3))))

test9 = TestCase (assertBool "Tests that sampleDict2 does not contain \"xyz\"."
                             (not (hasGen sampleDict2 "xyz")))

test10 = TestCase (assertBool "Tests that sampleDict3 does not contain \"xyz\"."
                              (not (hasGen sampleDict3 "xyz")))

-- Can check if an element is present.

test11 = TestCase (assertBool "Tests that sampleDict1 contains \"abc\"."
                              (hasGen sampleDict1 (fst gen1)))

test12 = TestCase (assertBool "Tests that sampleDict2 contains \"abc\"."
                              (hasGen sampleDict2 (fst gen1)))

test13 = TestCase (assertBool "Tests that sampleDict2 contains \"def\"."
                              (hasGen sampleDict2 (fst gen2)))

test14 = TestCase (assertBool "Tests that sampleDict3 contains \"abc\"."
                              (hasGen sampleDict3 (fst gen1)))

test15 = TestCase (assertBool "Tests that sampleDict3 contains \"def\"."
                              (hasGen sampleDict3 (fst gen2)))

test16 = TestCase (assertBool "Tests that sampleDict3 contains \"ab_12_ef\"."
                              (hasGen sampleDict3 (fst gen3)))

-- Can look up semantic values.

test17 = TestCase (assertEqual "Tests that sampleDict1 maps \"abc\" to 1."
                               (snd gen1)
                               (interpretGen sampleDict1 (fst gen1)))

test18 = TestCase (assertEqual "Tests that sampleDict2 maps \"abc\" to 1."
                               (snd gen1)
                               (interpretGen sampleDict2 (fst gen1)))

test19 = TestCase (assertEqual "Tests that sampleDict2 maps \"def\" to 2."
                               (snd gen2)
                               (interpretGen sampleDict2 (fst gen2)))

test20 = TestCase (assertEqual "Tests that sampleDict3 maps \"abc\" to 1."
                               (snd gen1)
                               (interpretGen sampleDict3 (fst gen1)))

test21 = TestCase (assertEqual "Tests that sampleDict3 maps \"def\" to 2."
                               (snd gen2)
                               (interpretGen sampleDict3 (fst gen2)))

test22 = TestCase (assertEqual "Tests that sampleDict3 maps \"ab_12_ef\" to Nothing."
                               (snd gen3)
                               (interpretGen sampleDict3 (fst gen3)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GenDictDoesNotContain_Test0" test1,
                                     TestLabel "GenDictDoesNotContain_Test1" test2,
                                     TestLabel "GenDictDoesNotContain_Test2" test3,
                                     TestLabel "GenDictDoesNotContain_Test3" test4,
                                     TestLabel "GenDictDoesNotContain_Test4" test5,
                                     TestLabel "GenDictDoesNotContain_Test5" test6,
                                     TestLabel "GenDictDoesNotContain_Test6" test7,
                                     TestLabel "GenDictDoesNotContain_Test7" test8,
                                     TestLabel "GenDictDoesNotContain_Test8" test9,
                                     TestLabel "GenDictDoesNotContain_Test9" test10,
                                     TestLabel "GenDictContains_Test0" test11,
                                     TestLabel "GenDictContains_Test1" test12,
                                     TestLabel "GenDictContains_Test2" test13,
                                     TestLabel "GenDictContains_Test3" test14,
                                     TestLabel "GenDictContains_Test4" test15,
                                     TestLabel "GenDictContains_Test5" test16,
                                     TestLabel "GenDictSemv_Test0" test17,
                                     TestLabel "GenDictSemv_Test1" test18,
                                     TestLabel "GenDictSemv_Test2" test19,
                                     TestLabel "GenDictSemv_Test3" test20,
                                     TestLabel "GenDictSemv_Test4" test21,
                                     TestLabel "GenDictSemv_Test5" test22]

main = defaultMain tests
