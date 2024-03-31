module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Tietze.Common
import Tietze.Generators.Categories
import Tietze.Generators.Semantics

-----------------------------------------------------------------------------------------
-- Generator Dictionaries.

-- This test builds up a generator dictionary and then checks properties. These tests are
-- agnostic to the implementation of GenDict, so will be useful even if the
-- implementation changes.

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

-- Can accumulate.

accumulator1 :: (String, Maybe Int) -> Maybe Int -> Maybe Int
accumulator1 (name, Just x)  (Just y) = Just (x + y)
accumulator1 _               Nothing  = Nothing
accumulator1 (_, Nothing)    _        = Nothing

test23 = TestCase (assertEqual "Folding accumulator1 on sampleDict0 is successful."
                               (Just 5)
                               (foldGens accumulator1 (Just 5) sampleDict0))

test24 = TestCase (assertEqual "Folding accumulator1 on sampleDict1 is successful."
                               (Just 6)
                               (foldGens accumulator1 (Just 5) sampleDict1))

test25 = TestCase (assertEqual "Folding accumulator1 on sampleDict2 is successful."
                               (Just 8)
                               (foldGens accumulator1 (Just 5) sampleDict2))

test26 = TestCase (assertEqual "Folding accumulator1 on sampleDict3 is successful."
                               Nothing
                               (foldGens accumulator1 (Just 5) sampleDict3))

accumulator2 :: (String, Maybe Int) -> Maybe Int -> Maybe Int
accumulator2 ("abc", _) (Just y) = Just (10 + y)
accumulator2 gen        acc      = (accumulator1 gen acc)

test27 = TestCase (assertEqual "Folding accumulator2 on sampleDict0 is successful."
                               (Just 5)
                               (foldGens accumulator2 (Just 5) sampleDict0))

test28 = TestCase (assertEqual "Folding accumulator2 on sampleDict1 is successful."
                               (Just 15)
                               (foldGens accumulator2 (Just 5) sampleDict1))

test29 = TestCase (assertEqual "Folding accumulator2 on sampleDict2 is successful."
                               (Just 17)
                               (foldGens accumulator2 (Just 5) sampleDict2))

test30 = TestCase (assertEqual "Folding accumulator2 on sampleDict3 is successful."
                               Nothing
                               (foldGens accumulator2 (Just 5) sampleDict3))

-- Can convert to an alphabet.

test31 = TestCase (assertEqual "Can extract alphabet from empty GenDict."
                               []
                               (toAlphabet sampleDict0))

test32 = TestCase (assertEqual "Can extract alphabet from sampleDict0."
                               ["abc"]
                               (toAlphabet sampleDict1))

test33 = TestCase (assertEqual "Can extract alphabet from sampleDict1."
                               ["abc", "def"]
                               (toAlphabet sampleDict2))

test34 = TestCase (assertEqual "Can extract alphabet from sampleDict2."
                               ["ab_12_ef", "abc", "def"]
                               (toAlphabet sampleDict3))

-----------------------------------------------------------------------------------------
-- Monoid semantics used in the test.

data ZeroInt = ZeroInt Int deriving (Show, Eq)

instance MonoidObj (ZeroInt) where
    equate   a           b           = Just (a == b)
    compose  (ZeroInt x) (ZeroInt y) = Just (ZeroInt (x * y))
    identity                         = ZeroInt 0

data BadInt = BadInt Int deriving (Show, Eq)

instance MonoidObj (BadInt) where
    equate   a          b          = Just (a == b)
    compose  (BadInt x) (BadInt y) = Just (BadInt (x * y))
    identity                       = BadInt 5

-----------------------------------------------------------------------------------------
-- Defines generators and words.

genA = "a"
genB = "b"
genC = "c"
genD = "d"
genE = "e"
genF = "f"

multIntGens = empty `addGen` (genA, Just (MultInt 1))
                    `addGen` (genB, Just (MultInt 2))
                    `addGen` (genC, Just (MultInt 3))
                    `addGen` (genD, Just (MultInt 6))
                    `addGen` (genE, Just (MultInt 9))
                    `addGen` (genF, Nothing)

addIntGens = empty `addGen` (genA, Just (AddInt 1))
                   `addGen` (genB, Just (AddInt 2))
                   `addGen` (genC, Just (AddInt 3))
                   `addGen` (genD, Just (AddInt 6))
                   `addGen` (genE, Just (AddInt 9))
                   `addGen` (genF, Nothing)

zeroIntGens = empty `addGen` (genA, Just (ZeroInt 1))
                    `addGen` (genB, Just (ZeroInt 2))
                    `addGen` (genC, Just (ZeroInt 3))
                    `addGen` (genD, Just (ZeroInt 6))
                    `addGen` (genE, Just (ZeroInt 9))
                    `addGen` (genF, Nothing)

symA = Symbol genA []
symB = Symbol genB []
symC = Symbol genC []
symD = Symbol genD []
symE = Symbol genE []
symF = Symbol genF []

wordA = [symA, symE, symA]              -- 1*9*1     = 9
wordB = [symC, symC, symA]              -- 3*3*1     = 9
wordC = [symC, symD, symF, symD]        -- 3*6*??*6  = ??
wordD = [symB, symB, symC, symB]        -- 2*2*3*2   = 24
wordE = [symA, symB, symD, symB, symA]  -- 1*2*6*2*1 = 24
wordF = [symD, symB]                    -- 6*2       = 12
wordG = [symC, symD, symE, symE]        -- 3*6*9*9   = 1458

-----------------------------------------------------------------------------------------
-- Tests: semEval

test35 = TestCase (assertEqual "Empty string with id 1 had value 1."
                               (Just (MultInt 1))
                               (semEval multIntGens []))

test36 = TestCase (assertEqual "Empty string with id 5 had value 5."
                               (Just (BadInt 5))
                               (semEval (empty :: GenDict BadInt) []))

test37 = TestCase (assertEqual "Tests evaluation of strings with multiplication (1/7)."
                               (Just (MultInt 9))
                               (semEval multIntGens wordA))

test38 = TestCase (assertEqual "Tests evaluation of strings with multiplication (3/7)."
                               (Just (MultInt 9))
                               (semEval multIntGens wordB))

test39 = TestCase (assertEqual "Tests evaluation of strings with multiplication (3/7)."
                               Nothing
                               (semEval multIntGens wordC))

test40 = TestCase (assertEqual "Tests evaluation of strings with multiplication (4/7)."
                               (Just (MultInt 24))
                               (semEval multIntGens wordD))

test41 = TestCase (assertEqual "Tests evaluation of strings with multiplication (5/7)."
                               (Just (MultInt 24))
                               (semEval multIntGens wordE))

test42 = TestCase (assertEqual "Tests evaluation of strings with multiplication (6/7)."
                               (Just (MultInt 12))
                               (semEval multIntGens wordF))

test43 = TestCase (assertEqual "Tests evaluation of strings with multiplication (7/7)."
                               (Just (MultInt 1458))
                               (semEval multIntGens wordG))

test44 = TestCase (assertEqual "Tests evaluation of strings with addition (1/2)."
                               (Just (AddInt 11))
                               (semEval addIntGens wordA))

test45 = TestCase (assertEqual "Tests evaluation of strings with addition (2/2)."
                               (Just (AddInt 7))
                               (semEval addIntGens wordB))

-----------------------------------------------------------------------------------------
-- Tests: semComp

test46 = TestCase (assertEqual "Tests semantic comparison of empty strings (1/2)."
                               (Just True)
                               (semComp multIntGens [] []))

test47 = TestCase (assertEqual "Tests semantic comparison of empty strings (2/2)."
                               (Just True)
                               (semComp (empty :: GenDict BadInt) [] []))

test48 = TestCase (assertEqual "Tests semantic comparison is with respect to id element."
                               (Just True)
                               (semComp zeroIntGens wordA wordD))

test49 = TestCase (assertEqual "Tests semantic comparison of equal strings (1/2)."
                               (Just True)
                               (semComp multIntGens wordA wordB))

test50 = TestCase (assertEqual "Tests semantic comparison of equal strings (2/2)."
                               (Just True)
                               (semComp multIntGens wordD wordE))

test51 = TestCase (assertEqual "Tests semantic comparison of strings not equal (1/3)."
                               (Just False)
                               (semComp multIntGens wordA wordD))

test52 = TestCase (assertEqual "Tests semantic comparison of strings not equal (2/3)."
                               (Just False)
                               (semComp multIntGens wordA wordE))

test53 = TestCase (assertEqual "Tests semantic comparison of strings not equal (3/3)."
                               (Just False)
                               (semComp multIntGens wordA wordF))

test54 = TestCase (assertEqual "Tests semantic comparison with missing generators (1/3)."
                               Nothing
                               (semComp multIntGens wordA wordC))

test55 = TestCase (assertEqual "Tests semantic comparison with missing generators (2/3)."
                               Nothing
                               (semComp multIntGens wordC wordA))

test56 = TestCase (assertEqual "Tests semantic comparison with missing generators (3/3)."
                               Nothing
                               (semComp multIntGens wordC wordC))

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
                                     TestLabel "GenDictSemv_Test5" test22,
                                     TestLabel "GenFolding_Test0" test23,
                                     TestLabel "GenFolding_Test1" test24,
                                     TestLabel "GenFolding_Test2" test25,
                                     TestLabel "GenFolding_Test3" test26,
                                     TestLabel "GenFolding_Test4" test27,
                                     TestLabel "GenFolding_Test5" test28,
                                     TestLabel "GenFolding_Test6" test29,
                                     TestLabel "GenFolding_Test7" test30,
                                     TestLabel "GenToAlpha_Test0" test31,
                                     TestLabel "GenToAlpha_Test1" test32,
                                     TestLabel "GenToAlpha_Test2" test33,
                                     TestLabel "GenToAlpha_Test3" test34,
                                     TestLabel "SemEval_ID_1" test35,
                                     TestLabel "SemEval_ID_2" test36,
                                     TestLabel "SemEval_Mult_1" test37,
                                     TestLabel "SemEval_Mult_2" test38,
                                     TestLabel "SemEval_Mult_3" test39,
                                     TestLabel "SemEval_Mult_4" test40,
                                     TestLabel "SemEval_Mult_5" test41,
                                     TestLabel "SemEval_Mult_6" test42,
                                     TestLabel "SemEval_Mult_7" test43,
                                     TestLabel "SemEval_Add_1" test44,
                                     TestLabel "SemEval_Add_2" test45,
                                     TestLabel "SemComp_Empty_1" test46,
                                     TestLabel "SemComp_Empty_2" test47,
                                     TestLabel "SemComp_ID" test48,
                                     TestLabel "SemComp_EQ_1" test49,
                                     TestLabel "SemComp_EQ_2" test50,
                                     TestLabel "SemComp_NEQ_1" test51,
                                     TestLabel "SemComp_NEQ_2" test52,
                                     TestLabel "SemComp_NEQ_3" test53,
                                     TestLabel "SemComp_Missing_1" test54,
                                     TestLabel "SemComp_Missing_2" test55,
                                     TestLabel "SemComp_Missing_3" test56]

main = defaultMain tests
