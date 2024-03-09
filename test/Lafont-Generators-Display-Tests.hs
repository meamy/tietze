module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Lafont.Common
import Lafont.Generators.Categories
import Lafont.Generators.Algebraic.ModP
import Lafont.Generators.Algebraic.Product
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
-- Display MultModP and AddModP.

test13 = TestCase (assertEqual "Can display AddInt (1/2)."
                               "5"
                               (display (AddInt 5)))

test14 = TestCase (assertEqual "Can display AddInt (2/2)."
                               "-30"
                               (display (AddInt (-30))))

test15 = TestCase (assertEqual "Can display AddInt (1/2)."
                               "5"
                               (display (MultInt 5)))

test16 = TestCase (assertEqual "Can display AddInt (2/2)."
                               "-30"
                               (display (MultInt (-30))))

test17 = TestCase (assertEqual "Can display ArithModP for p > 0 (1/2)."
                               "2 mod 3"
                               (display val))
    where val =  fromJust $ inclusionModP (MultInt 5) 3

test18 = TestCase (assertEqual "Can display ArithModP for p > 0 (2/2)."
                               "1 mod 3"
                               (display val))
    where val =  fromJust $ inclusionModP (AddInt (-5)) 3

test19 = TestCase (assertEqual "Can display ArithModP for p = 0 (1/2)."
                               "20"
                               (display val))
    where val =  fromJust $ inclusionModP (MultInt 20) 0

test20 = TestCase (assertEqual "Can display ProductType (1/3)."
                               "()"
                               (display (promoteToProduct list)))
    where list = [] :: [MultInt]

test21 = TestCase (assertEqual "Can display ProductType (2/3)."
                               "(1)"
                               (display (promoteToProduct list)))
    where list = [MultInt 1]

test22 = TestCase (assertEqual "Can display ProductType (2/3)."
                               "(1,2,3)"
                               (display (promoteToProduct list)))
    where list = [AddInt 1, AddInt 2, AddInt 3]

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
                                     TestLabel "Display_Matrix_2" test12,
                                     TestLabel "Display_AddInt_1" test13,
                                     TestLabel "Display_AddInt_2" test14,
                                     TestLabel "Display_MultInt_1" test15,
                                     TestLabel "Display_MultInt_2" test16,
                                     TestLabel "Display_ArithModP_0" test17,
                                     TestLabel "Display_ArithModP_1" test18,
                                     TestLabel "Display_ArithMod0" test19,
                                     TestLabel "Display_ProductType_1" test20,
                                     TestLabel "Display_ProductType_2" test21,
                                     TestLabel "Display_ProductType_3" test22]

main = defaultMain tests
