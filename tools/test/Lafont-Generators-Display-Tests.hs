module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Lafont.Common
import Lafont.Generators.Display

-----------------------------------------------------------------------------------------
-- Display Dyadic Integers.

dyadic9 = Dyadic 16 4
dyadic10 = Dyadic 24 4

test1 = TestCase (assertEqual "Can print integral dyadics (1/2)."
                              "7"
                              (display (Dyadic 7 0)))

test2 = TestCase (assertEqual "Can print integral dyadics (2/2)."
                              "10"
                              (display (Dyadic 10 0)))

test3 = TestCase (assertEqual "Can print zero dyadics (1/2)."
                              "0"
                              (display (Dyadic 0 0)))

test4 = TestCase (assertEqual "Can print zero dyadics (2/2)."
                              "0"
                              (display (Dyadic 0 10)))

test5 = TestCase (assertEqual "Can print one-half dyadics (1/2)."
                              "3/2"
                              (display (Dyadic 3 1)))

test6 = TestCase (assertEqual "Can print one-half dyadics (1/2)."
                              "5/2"
                              (display (Dyadic 5 1)))

test7 = TestCase (assertEqual "Can print powers of one-half (1/2)."
                              "3/2^2"
                              (display (Dyadic 3 2)))

test8 = TestCase (assertEqual "Can print powers of one-half (2/2)."
                              "5/2^3"
                              (display (Dyadic 5 3)))

test9 = TestCase (assertEqual "Can reduce dyadics (1/2)."
                              "1"
                              (display (Dyadic 16 4)))

test10 = TestCase (assertEqual "Can reduce dyadics (2/2)."
                               "3/2"
                               (display (Dyadic 24 4)))

-----------------------------------------------------------------------------------------
-- Display Matrices.

mat1 = matrix2x2 ((Dyadic 1 2), (Dyadic 0 3))
                 ((Dyadic 5 0), (Dyadic 6 4))

mat2 = matrix3x3 ((Dyadic 1 2), (Dyadic 0 3), (Dyadic 24 4))
                 ((Dyadic 5 0), (Dyadic 6 4), (Dyadic 0 10))
                 ((Dyadic 7 0), (Dyadic 0 0), (Dyadic 10 0))

test11 = TestCase (assertEqual "Can display 2x2 matrices."
                               "[[1/2^2, 0], [5, 3/2^3]]"
                               (display mat1))

test12 = TestCase (assertEqual "Can display 3x3 matrices."
                               "[[1/2^2, 0, 3/2], [5, 3/2^3, 0], [7, 0, 10]]"
                               (display mat2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Display_Dyadic_1" test1,
                                     TestLabel "Display_Dyadic_2" test2,
                                     TestLabel "Display_Dyadic_3" test3,
                                     TestLabel "Display_Dyadic_4" test4,
                                     TestLabel "Display_Dyadic_5" test5,
                                     TestLabel "Display_Dyadic_6" test6,
                                     TestLabel "Display_Dyadic_7" test7,
                                     TestLabel "Display_Dyadic_8" test8,
                                     TestLabel "Display_Dyadic_9" test9,
                                     TestLabel "Display_Dyadic_10" test10,
                                     TestLabel "Display_Matrix_1" test11,
                                     TestLabel "Display_Matrix_2" test12]

main = defaultMain tests
