module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quantum.Synthesis.Ring
import Tietze.Ring

-----------------------------------------------------------------------------------------
-- reduceDyadic

test1 = TestCase (assertEqual "Dyadic reduction base case, n=0 (1/3)."
                              (Dyadic 0 0)
                              (reduceDyadic (Dyadic 0 0)))

test2 = TestCase (assertEqual "Dyadic reduction base case, n=0 (2/3)."
                              (Dyadic 3 0)
                              (reduceDyadic (Dyadic 3 0)))

test3 = TestCase (assertEqual "Dyadic reduction base case, n=0 (3/3)."
                              (Dyadic 4 0)
                              (reduceDyadic (Dyadic 4 0)))

test4 = TestCase (assertEqual "Dyadic reduction base case, a has no even divisors (1/3)."
                              (Dyadic 3 3)
                              (reduceDyadic (Dyadic 3 3)))

test5 = TestCase (assertEqual "Dyadic reduction base case, a has no even divisors (2/3)."
                              (Dyadic 5 3)
                              (reduceDyadic (Dyadic 5 3)))

test6 = TestCase (assertEqual "Dyadic reduction base case, a has no even divisors (3/3)."
                              (Dyadic 27 8)
                              (reduceDyadic (Dyadic 27 8)))

test7 = TestCase (assertEqual "Dyadic reduction recursive case, 1 iteration."
                              (Dyadic 3 2)
                              (reduceDyadic (Dyadic 6 3)))

test8 = TestCase (assertEqual "Dyadic reduction recursive case, 2 iterations."
                              (Dyadic 15 6)
                              (reduceDyadic (Dyadic 60 8)))

test9 = TestCase (assertEqual "Dyadic reduction recursive case, 3 iterations."
                              (Dyadic 7 9)
                              (reduceDyadic (Dyadic 56 12)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "reduceDyadic_0Exp_1" test1,
                                     TestLabel "reduceDyadic_0Exp_2" test2,
                                     TestLabel "reduceDyadic_0Exp_3" test3,
                                     TestLabel "reduceDyadic_NoEvenFactors_1" test4,
                                     TestLabel "reduceDyadic_NoEvenFactors_2" test5,
                                     TestLabel "reduceDyadic_NoEvenFactors_3" test6,
                                     TestLabel "reduceDyadic_1Step" test7,
                                     TestLabel "reduceDyadic_2Step" test8,
                                     TestLabel "reduceDyadic_3Step" test9]

main = defaultMain tests
