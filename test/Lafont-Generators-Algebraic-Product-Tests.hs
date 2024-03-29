module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Tietze.Generators.Algebraic.Internal.Product
import Tietze.Generators.Algebraic.Product
import Tietze.Generators.Categories

-----------------------------------------------------------------------------------------
-- promoteToProduct

type IntProd = ProductType AddInt

empty = [] :: [AddInt]
singleton = [AddInt 5]
triple = [AddInt 5, AddInt 20, AddInt (-5)]

test1 = TestCase (assertEqual "promoteToProduct supports empty lists."
                              (ProductVal empty)
                              (promoteToProduct empty))

test2 = TestCase (assertEqual "promoteToProduct supports singleton lists."
                              (ProductVal singleton)
                              (promoteToProduct singleton))

test3 = TestCase (assertEqual "promoteToProduct supports three element lists."
                              (ProductVal triple)
                              (promoteToProduct triple))

-----------------------------------------------------------------------------------------
-- expandIdentityFor

test4 = TestCase (assertEqual "expandIdentityFor supports empty lists."
                              empty
                              (expandIdentityFor empty))

test5 = TestCase (assertEqual "expandIdentityFor supports singleton lists."
                              [AddInt 0]
                              (expandIdentityFor singleton))

test6 = TestCase (assertEqual "expandIdentityFor supports three element lists."
                              [AddInt 0, AddInt 0, AddInt 0]
                              (expandIdentityFor triple))

-----------------------------------------------------------------------------------------
-- identity

test7 = TestCase (assertEqual "Correct identity for ProductType."
                              (ProductID :: IntProd)
                              (identity :: IntProd))

-----------------------------------------------------------------------------------------
-- equate

addIntID :: IntProd
addIntID = ProductID

test8 = TestCase (assertEqual "The value ProductID can be equated to identities (1/3)."
                              (Just True :: Maybe Bool)
                              (equate addIntID addIntID))

test9 = TestCase (assertEqual "The value ProductID can be equated to identities (2/3)."
                              (Just True :: Maybe Bool)
                              (equate (ProductVal []) addIntID))

test10 = TestCase (assertEqual "The value ProductID can be equated to identities (3/3)."
                               (Just True :: Maybe Bool)
                               (equate addIntID x))
    where x = ProductVal [AddInt 0, AddInt 0, AddInt 0]

test11 = TestCase (assertEqual "Equating ProductID with non-identity fails (1/2)."
                               (Just False :: Maybe Bool)
                               (equate addIntID x))
    where x = ProductVal [AddInt 0, AddInt 5, AddInt (-24)]

test12 = TestCase (assertEqual "Equating ProductID with non-identity fails (2/2)."
                               (Just False :: Maybe Bool)
                               (equate x addIntID))
    where x = ProductVal [AddInt 0, AddInt 0, AddInt 5, AddInt (-24)]

test13 = TestCase (assertEqual "Equating valid ProductType values works (1/3)."
                               (Just True :: Maybe Bool)
                               (equate x x))
    where x = ProductVal [] :: IntProd

test14 = TestCase (assertEqual "Equating valid ProductType values works (2/3)."
                               (Just True :: Maybe Bool)
                               (equate x x))
    where x = ProductVal [AddInt 5, AddInt 10, AddInt (-5)]

test15 = TestCase (assertEqual "Equating valid ProductType values works (3/3)."
                               (Just False :: Maybe Bool)
                               (equate x y))
    where x = ProductVal [AddInt 5, AddInt 10, AddInt (-5)]
          y = ProductVal [AddInt 5, AddInt 10, AddInt 5]

test16 = TestCase (assertEqual "Equating detects incomparable values (1/2)."
                               (Nothing :: Maybe Bool)
                               (equate x y))
    where x = ProductVal [AddInt 5, AddInt 10, AddInt (-5)]
          y = ProductVal [AddInt 5, AddInt 10]

test17 = TestCase (assertEqual "Equating detects incomparable values (2/2)."
                               (Nothing :: Maybe Bool)
                               (equate x y))
    where x = ProductVal [AddInt 5, AddInt 10]
          y = ProductVal [AddInt 5, AddInt 10, AddInt (-5)]

-----------------------------------------------------------------------------------------
-- compose

test18 = TestCase (assertEqual "The value ArithID can be composed (1/3)."
                               (Just addIntID :: Maybe IntProd)
                               (compose addIntID addIntID))

test19 = TestCase (assertEqual "The value ArithID can be equated to identities (2/3)."
                               (Just x :: Maybe IntProd)
                               (compose x addIntID))
    where x = ProductVal [AddInt 5, AddInt 10]

test20 = TestCase (assertEqual "The value ArithID can be equated to identities (3/3)."
                               (Just x :: Maybe IntProd)
                               (compose addIntID x))
    where x = ProductVal [AddInt 5, AddInt 10, AddInt (-5)]

test21 = TestCase (assertEqual "Composing valid ProductType values works (1/3)."
                               (Just x :: Maybe IntProd)
                               (compose x x))
    where x = ProductVal empty

test22 = TestCase (assertEqual "Composing valid ProductType values works (2/3)."
                               (Just z :: Maybe IntProd)
                               (compose x y))
    where x = ProductVal [AddInt 5, AddInt 10]
          y = ProductVal [AddInt 2, AddInt (-5)]
          z = ProductVal [AddInt 7, AddInt 5]

test23 = TestCase (assertEqual "Composing valid ProductType values works (3/3)."
                               (Just z :: Maybe IntProd)
                               (compose x y))
    where x = ProductVal [AddInt 3, AddInt 22, AddInt 32]
          y = ProductVal [AddInt 10, AddInt (-3), AddInt 77]
          z = ProductVal [AddInt 13, AddInt 19, AddInt 109]

test24 = TestCase (assertEqual "Composing detects incomparable values (1/2)."
                               (Nothing :: Maybe IntProd)
                               (compose x y))
    where x = ProductVal empty
          y = ProductVal [AddInt 5, AddInt 10]

test25 = TestCase (assertEqual "Composing detects incomparable values (2/2)."
                               (Nothing :: Maybe IntProd)
                               (compose x y))
    where x = ProductVal [AddInt 5, AddInt 10]
          y = ProductVal [AddInt 13, AddInt 19, AddInt 109]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "promoteToProduct_Empty" test1,
                                     TestLabel "promoteToProduct_Singleton" test2,
                                     TestLabel "promoteToProduct_Triple" test3,
                                     TestLabel "expandIdentityFor_Empty" test4,
                                     TestLabel "expandIdentityFor_Singleton" test5,
                                     TestLabel "expandIdentityFor_Triple" test6,
                                     TestLabel "identity" test7,
                                     TestLabel "equate_ProductID_True_1" test8,
                                     TestLabel "equate_ProductID_True_2" test9,
                                     TestLabel "equate_ProductID_True_3" test10,
                                     TestLabel "equate_ProductID_False_1" test11,
                                     TestLabel "equate_ProductID_False_2" test12,
                                     TestLabel "equate_Valid_1" test13,
                                     TestLabel "equate_Valid_2" test14,
                                     TestLabel "equate_Valid_3" test15,
                                     TestLabel "equate_Invalid_3" test16,
                                     TestLabel "equate_Invalid_3" test17,
                                     TestLabel "compose_ProductID_ProductID" test18,
                                     TestLabel "compose_ProductID_Valid_1" test19,
                                     TestLabel "compose_ProductID_Valid_2" test20,
                                     TestLabel "compose_Valid_1" test21,
                                     TestLabel "compose_Valid_2" test22,
                                     TestLabel "compose_Valid_3" test23,
                                     TestLabel "compose_Invalid_2" test24,
                                     TestLabel "compose_Invalid_3" test25]

main = defaultMain tests
