module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Tietze.Generators.Categories
import Tietze.Generators.Algebraic.Internal.ModP
import Tietze.Generators.Algebraic.ModP

-----------------------------------------------------------------------------------------
-- AddInt: modp

test1 = TestCase (assertEqual "AddInt supports modp (1/9)."
                              (AddInt 5)
                              ((AddInt 5) `pmod` 10))

test2 = TestCase (assertEqual "AddInt supports modp (2/9)."
                              (AddInt 7)
                              ((AddInt 7) `pmod` 10))

test3 = TestCase (assertEqual "AddInt supports modp (3/9)."
                              (AddInt 9)
                              ((AddInt 9) `pmod` 10))

test4 = TestCase (assertEqual "AddInt supports modp (4/9)."
                              (AddInt 5)
                              ((AddInt 5) `pmod` 8))

test5 = TestCase (assertEqual "AddInt supports modp (5/9)."
                              (AddInt 7)
                              ((AddInt 7) `pmod` 8))

test6 = TestCase (assertEqual "AddInt supports modp (6/9)."
                              (AddInt 1)
                              ((AddInt 9) `pmod` 8))

test7 = TestCase (assertEqual "AddInt supports modp (7/9)."
                              (AddInt 5)
                              ((AddInt 5) `pmod` 6))

test8 = TestCase (assertEqual "AddInt supports modp (8/9)."
                              (AddInt 1)
                              ((AddInt 7) `pmod` 6))

test9 = TestCase (assertEqual "AddInt supports modp (9/9)."
                              (AddInt 4)
                              ((AddInt 10) `pmod` 6))

-----------------------------------------------------------------------------------------
-- MultInt: modp

test10 = TestCase (assertEqual "MultInt supports modp (1/9)."
                               (MultInt 5)
                               ((MultInt 5) `pmod` 10))

test11 = TestCase (assertEqual "MultInt supports modp (2/9)."
                               (MultInt 7)
                               ((MultInt 7) `pmod` 10))

test12 = TestCase (assertEqual "MultInt supports modp (3/9)."
                               (MultInt 9)
                               ((MultInt 9) `pmod` 10))

test13 = TestCase (assertEqual "MultInt supports modp (4/9)."
                               (MultInt 5)
                               ((MultInt 5) `pmod` 8))

test14 = TestCase (assertEqual "MultInt supports modp (5/9)."
                               (MultInt 7)
                               ((MultInt 7) `pmod` 8))

test15 = TestCase (assertEqual "MultInt supports modp (6/9)."
                               (MultInt 1)
                               ((MultInt 9) `pmod` 8))

test16 = TestCase (assertEqual "MultInt supports modp (7/9)."
                               (MultInt 5)
                               ((MultInt 5) `pmod` 6))

test17 = TestCase (assertEqual "MultInt supports modp (8/9)."
                               (MultInt 1)
                               ((MultInt 7) `pmod` 6))

test18 = TestCase (assertEqual "MultInt supports modp (9/9)."
                               (MultInt 4)
                               ((MultInt 10) `pmod` 6))

-----------------------------------------------------------------------------------------
-- reduce

test19 = TestCase (assertEqual "Reduction mod 0 is a no-op (1/2)"
                               (ArithModP (AddInt 100) 0)
                               (reduce (ArithModP (AddInt 100) 0)))

test20 = TestCase (assertEqual "Reduction mod 0 is a no-op (2/2)"
                               (ArithModP (AddInt 200) 0)
                               (reduce (ArithModP (AddInt 200) 0)))

test21 = TestCase (assertEqual "Reduction mod 5 is a no-op (1/2)"
                               (ArithModP (AddInt 4) 5)
                               (reduce (ArithModP (AddInt 4) 5)))

test22 = TestCase (assertEqual "Reduction mod 5 is a no-op (2/2)"
                               (ArithModP (AddInt 2) 5)
                               (reduce (ArithModP (AddInt 7) 5)))

test23 = TestCase (assertEqual "Reduction mod 7 is a no-op (1/2)"
                               (ArithModP (AddInt 4) 7)
                               (reduce (ArithModP (AddInt 4) 7)))

test24 = TestCase (assertEqual "Reduction mod 7 is a no-op (2/2)"
                               (ArithModP (AddInt 2) 7)
                               (reduce (ArithModP (AddInt 9) 7)))

-----------------------------------------------------------------------------------------
-- inclusionModP

type AddIntModP = ArithModP AddInt

test25 = TestCase (assertEqual "Inclusion modulo 0 works."
                               (Just (ArithModP (AddInt 100) 0) :: Maybe AddIntModP)
                               (inclusionModP (AddInt 100) 0))

test26 = TestCase (assertEqual "Inclusion modulo 11 works (1/2)."
                               (Just (ArithModP (AddInt 7) 11) :: Maybe AddIntModP)
                               (inclusionModP (AddInt 7) 11))

test27 = TestCase (assertEqual "Inclusion modulo 11 works (2/2)."
                               (Just (ArithModP (AddInt 7) 11) :: Maybe AddIntModP)
                               (inclusionModP (AddInt 117) 11))

test28 = TestCase (assertEqual "Inclusion modulo a negative value fails"
                               (Nothing :: Maybe AddIntModP)
                               (inclusionModP (AddInt 117) (-5)))

-----------------------------------------------------------------------------------------
-- identity

test29 = TestCase (assertEqual "Correct identity for ArithModP."
                               ArithID
                               (identity :: AddIntModP))

-----------------------------------------------------------------------------------------
-- equate

addIntID :: AddIntModP
addIntID = ArithID

test30 = TestCase (assertEqual "The value ArithID can be equated to identities (1/3)."
                               (Just True :: Maybe Bool)
                               (equate addIntID addIntID))

test31 = TestCase (assertEqual "The value ArithID can be equated to identities (2/3)."
                               (Just True :: Maybe Bool)
                               (equate (ArithModP (AddInt 0) 0) addIntID))

test32 = TestCase (assertEqual "The value ArithID can be equated to identities (3/3)."
                               (Just True :: Maybe Bool)
                               (equate addIntID (ArithModP (AddInt 0) 5)))

test33 = TestCase (assertEqual "The value ArithID fails when equated with bad p (1/2)."
                               (Nothing :: Maybe Bool)
                               (equate addIntID (ArithModP (AddInt 0) (-5))))

test34 = TestCase (assertEqual "The value ArithID fails when equated with bad p (2/2)."
                               (Nothing :: Maybe Bool)
                               (equate (ArithModP (AddInt 0) (-5)) addIntID))

test35 = TestCase (assertEqual "Equating ArithID with non-identity fails (1/2)."
                               (Just False :: Maybe Bool)
                               (equate addIntID (ArithModP (AddInt 55) 5)))

test36 = TestCase (assertEqual "Equating ArithID with non-identity fails (2/2)."
                               (Just False :: Maybe Bool)
                               (equate (ArithModP (AddInt 55) 5) addIntID))

test37 = TestCase (assertEqual "Equating valid ArithModP values works (1/3)."
                               (Just True :: Maybe Bool)
                               (equate x x))
    where x = ArithModP (AddInt 55) 5

test38 = TestCase (assertEqual "Equating valid ArithModP values works (2/3)."
                               (Just False :: Maybe Bool)
                               (equate x y))
    where x = ArithModP (AddInt 45) 5
          y = ArithModP (AddInt 55) 5

test39 = TestCase (assertEqual "Equating valid ArithModP values works (2/3)."
                               (Just False :: Maybe Bool)
                               (equate y x))
    where x = ArithModP (AddInt 45) 5
          y = ArithModP (AddInt 55) 5

test40 = TestCase (assertEqual "Equating detects incomparable values (1/3)."
                               (Nothing :: Maybe Bool)
                               (equate y x))
    where x = ArithModP (AddInt 45) 4
          y = ArithModP (AddInt 55) 5

test41 = TestCase (assertEqual "Equating detects incomparable values (2/3)."
                               (Nothing :: Maybe Bool)
                               (equate y x))
    where x = ArithModP (AddInt 45) (-1)
          y = ArithModP (AddInt 55) 5

test42 = TestCase (assertEqual "Equating detects incomparable values (3/3)."
                               (Nothing :: Maybe Bool)
                               (equate y x))
    where x = ArithModP (AddInt 45) 5
          y = ArithModP (AddInt 55) (-1)

-----------------------------------------------------------------------------------------
-- compose

test43 = TestCase (assertEqual "The value ArithID can be composed (1/3)."
                               (Just addIntID :: Maybe AddIntModP)
                               (compose addIntID addIntID))

test44 = TestCase (assertEqual "The value ArithID can be equated to identities (2/3)."
                               (Just x :: Maybe AddIntModP)
                               (compose x addIntID))
    where x = ArithModP (AddInt 55) 5

test45 = TestCase (assertEqual "The value ArithID can be equated to identities (3/3)."
                               (Just x :: Maybe AddIntModP)
                               (compose addIntID x))
    where x = ArithModP (AddInt 75) 0

test46 = TestCase (assertEqual "The value ArithID fails when composed with bad p (1/2)."
                               (Nothing :: Maybe AddIntModP)
                               (compose addIntID (ArithModP (AddInt 0) (-5))))

test47 = TestCase (assertEqual "The value ArithID fails when composed with bad p (2/2)."
                               (Nothing :: Maybe AddIntModP)
                               (compose (ArithModP (AddInt 0) (-5)) addIntID))

test48 = TestCase (assertEqual "Composing valid ArithModP values works (1/2)."
                               (Just (ArithModP (AddInt 1) 5) :: Maybe AddIntModP)
                               (compose x y))
    where x = ArithModP (AddInt 46) 5
          y = ArithModP (AddInt 55) 5

test49 = TestCase (assertEqual "Composing valid ArithModP values works (2/2)."
                               (Just (ArithModP (AddInt 2) 7) :: Maybe AddIntModP)
                               (compose y x))
    where x = ArithModP (AddInt 22) 7
          y = ArithModP (AddInt 15) 7

test50 = TestCase (assertEqual "Composing detects incomparable values (1/3)."
                               (Nothing :: Maybe AddIntModP)
                               (compose y x))
    where x = ArithModP (AddInt 45) 4
          y = ArithModP (AddInt 55) 5

test51 = TestCase (assertEqual "Composing detects incomparable values (2/3)."
                               (Nothing :: Maybe AddIntModP)
                               (compose y x))
    where x = ArithModP (AddInt 45) (-1)
          y = ArithModP (AddInt 55) 5

test52 = TestCase (assertEqual "Composing detects incomparable values (3/3)."
                               (Nothing :: Maybe AddIntModP)
                               (compose y x))
    where x = ArithModP (AddInt 45) 5
          y = ArithModP (AddInt 55) (-1)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "AddInt_modp_1" test1,
                                     TestLabel "AddInt_modp_2" test2,
                                     TestLabel "AddInt_modp_3" test3,
                                     TestLabel "AddInt_modp_4" test4,
                                     TestLabel "AddInt_modp_5" test5,
                                     TestLabel "AddInt_modp_6" test6,
                                     TestLabel "AddInt_modp_7" test7,
                                     TestLabel "AddInt_modp_8" test8,
                                     TestLabel "AddInt_modp_9" test9,
                                     TestLabel "MultInt_modp_1" test10,
                                     TestLabel "MultInt_modp_2" test11,
                                     TestLabel "MultInt_modp_3" test12,
                                     TestLabel "MultInt_modp_4" test13,
                                     TestLabel "MultInt_modp_5" test14,
                                     TestLabel "MultInt_modp_6" test15,
                                     TestLabel "MultInt_modp_7" test16,
                                     TestLabel "MultInt_modp_8" test17,
                                     TestLabel "MultInt_modp_9" test18,
                                     TestLabel "reduce_mod0_1" test19,
                                     TestLabel "reduce_mod0_2" test20,
                                     TestLabel "reduce_mod5_1" test21,
                                     TestLabel "reduce_mod5_2" test22,
                                     TestLabel "reduce_mod7_1" test23,
                                     TestLabel "reduce_mod7_2" test24,
                                     TestLabel "Inclusion_Mod0" test25,
                                     TestLabel "Inclusion_Mod11_1" test26,
                                     TestLabel "Inclusion_Mod11_2" test27,
                                     TestLabel "inclusionModP_Fails" test28,
                                     TestLabel "identity" test29,
                                     TestLabel "equate_ArithID_True_1" test30,
                                     TestLabel "equate_ArithID_True_2" test31,
                                     TestLabel "equate_ArithID_True_3" test32,
                                     TestLabel "equate_ArithID_BadP_1" test33,
                                     TestLabel "equate_ArithID_BadP_2" test34,
                                     TestLabel "equate_ArithID_False_1" test35,
                                     TestLabel "equate_ArithID_False_2" test36,
                                     TestLabel "equate_Valid_1" test37,
                                     TestLabel "equate_Valid_2" test38,
                                     TestLabel "equate_Valid_3" test39,
                                     TestLabel "equate_Invalid_1" test40,
                                     TestLabel "equate_Invalid_2" test41,
                                     TestLabel "equate_Invalid_3" test42,
                                     TestLabel "compose_ArithID_ArithID" test43,
                                     TestLabel "compose_ArithID_Valid_1" test44,
                                     TestLabel "compose_ArithID_Valid_2" test45,
                                     TestLabel "compose_ArithID_BadP_1" test46,
                                     TestLabel "compose_ArithID_BadP_2" test47,
                                     TestLabel "compose_Valid_1" test48,
                                     TestLabel "compose_Valid_2" test49,
                                     TestLabel "compose_Invalid_1" test50,
                                     TestLabel "compose_Invalid_2" test51,
                                     TestLabel "compose_Invalid_3" test52]

main = defaultMain tests
