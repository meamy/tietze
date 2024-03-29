module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Tietze.Generators.Categories
import Tietze.Generators.QubitGates

-----------------------------------------------------------------------------------------
-- Additive Integers (Monoid).

test1 = TestCase (assertEqual "Can determine the identity of the additive integers."
                              (AddInt 0)
                              (identity :: AddInt))

test2 = TestCase (assertEqual "Can compose additive integers (1/2)."
                              (Just (AddInt 12) :: Maybe AddInt)
                              (compose (AddInt 5) (AddInt 7)))

test3 = TestCase (assertEqual "Can compose additive integers (2/2)."
                              (Just (AddInt 76) :: Maybe AddInt)
                              (compose (AddInt 4) (AddInt 72)))

-----------------------------------------------------------------------------------------
-- Multiplicative Integers (Monoid).

test4 = TestCase (assertEqual "Can determine the identity of the multiplicative integers."
                              (MultInt 1)
                              (identity :: MultInt))

test5 = TestCase (assertEqual "Can compose multiplicative integers (1/2)."
                              (Just (MultInt 35) :: Maybe MultInt)
                              (compose (MultInt 5) (MultInt 7)))

test6 = TestCase (assertEqual "Can compose multiplicative integers (2/2)."
                              (Just (MultInt 40) :: Maybe MultInt)
                              (compose (MultInt 4) (MultInt 10)))

-----------------------------------------------------------------------------------------
-- Four-by-four Dyadic Matrices (Monoid).

test7 = TestCase (assertEqual "Can determine the identity of the 4x4 dyadic matrices."
                              (fromInteger 1 :: Matrix Four Four Dyadic)
                              (identity :: Matrix Four Four Dyadic))

test8 = TestCase (assertEqual "Can compose 4x4 dyadic matrices (1/2)."
                              expt
                              (compose gateCX gate))
    where gate = gateX `tensor` gateZ
          expt = Just (gateCX * gate) :: Maybe (Matrix Four Four Dyadic)

test9 = TestCase (assertEqual "Can compose 4x4 dyadic matrices (2/2)."
                              expt
                              (compose gateSwap gateK))
    where expt = Just (gateSwap * gateK) :: Maybe (Matrix Four Four Dyadic)


-----------------------------------------------------------------------------------------
-- Eight-by-eight Dyadic Matrices (Monoid).

test10 = TestCase (assertEqual "Can determine the identity of the 8x8 dyadic matrices."
                               (fromInteger 1 :: Matrix Eight Eight Dyadic)
                               (identity :: Matrix Eight Eight Dyadic))

test11 = TestCase (assertEqual "Can compose 8x8 dyadic matrices (1/2)."
                               expt
                               (compose gateTof gate))
    where gate = gateX `tensor` gateK
          expt = Just (gateTof * gate) :: Maybe (Matrix Eight Eight Dyadic)

test12 = TestCase (assertEqual "Can compose 8x8 dyadic matrices (2/2)."
                               expt
                               (compose gate1 gate2))
    where gate1 = gateX `tensor` gateCX
          gate2 = gateK `tensor` gateZ
          expt = Just (gate1 * gate2) :: Maybe (Matrix Eight Eight Dyadic)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "AddInt_Identity" test1,
                                     TestLabel "AddInt_Compose_1" test2,
                                     TestLabel "AddInt_Compose_2" test3,
                                     TestLabel "MultInt_Identity" test4,
                                     TestLabel "MultInt_Compose_1" test5,
                                     TestLabel "MultInt_Compose_2" test6,
                                     TestLabel "4x4Dyadic_Identity" test7,
                                     TestLabel "4x4Dyadic_Compose_1" test8,
                                     TestLabel "4x4Dyadic_Compose_2" test9,
                                     TestLabel "8x8Dyadic_Identity" test10,
                                     TestLabel "8x8Dyadic_Compose_1" test11,
                                     TestLabel "8x8Dyadic_Compose_2" test12]

main = defaultMain tests
