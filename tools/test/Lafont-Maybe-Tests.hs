module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Maybe

-----------------------------------------------------------------------------------------
-- maybeApply

nothingAsInt :: Maybe Int
nothingAsInt = Nothing

nothingAsBool :: Maybe Bool
nothingAsBool = Nothing

test1 = TestCase (assertEqual "Can use maybeApply to convert an integer to a string."
                               (Just "101" :: Maybe String)
                               (maybeApply (Just 101) show))

test2 = TestCase (assertEqual "Can use maybeApply to convert an Boolean to an integer."
                               (Just 1 :: Maybe Int)
                               (maybeApply (Just True) (\b -> if b then 1 else 0)))

test3 = TestCase (assertEqual "maybeApply can handle Nothing (1/2)."
                               Nothing
                               (maybeApply nothingAsInt show))

test4 = TestCase (assertEqual "maybeApply can handle Nothing (2/2)."
                               Nothing
                               (maybeApply nothingAsBool (\b -> if b then 1 else 0)))

-----------------------------------------------------------------------------------------
-- branchJust

f :: Int -> Maybe String
f 5 = Nothing
f n = Just (show n)

test5 = TestCase (assertEqual "Can use branchJust to convert an integer to a string."
                              (Just "101" :: Maybe String)
                              (branchJust (Just 101) f))

test6 = TestCase (assertEqual "Can use branchJust to convert an integer to Nothing."
                              Nothing
                              (branchJust (Just 5) f))

test7 = TestCase (assertEqual "branchJust can handle Nothing."
                              Nothing
                              (branchJust nothingAsInt f))

-----------------------------------------------------------------------------------------
-- branchNothing

test8 = TestCase (assertEqual "branchNothing can handle Just."
                              (Just 5 :: Maybe Int)
                              (branchNothing (Just 5) (Just 10)))

test9 = TestCase (assertEqual "branchNothing can handle Nothing."
                              (Just 10 :: Maybe Int)
                              (branchNothing nothingAsInt (Just 10)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "maybeApply_JustIntToJustString" test1,
                                     TestLabel "maybeApply_JustBoolToJustInt" test2,
                                     TestLabel "maybeApply_NothingAsMaybeInt" test3,
                                     TestLabel "maybeApply_NothingAsMaybeBool" test4,
                                     TestLabel "branchJust_Just_Just" test5,
                                     TestLabel "branchJust_Just_Nothing" test6,
                                     TestLabel "branchJust_Nothing" test7,
                                     TestLabel "branchNothing_Just" test8,
                                     TestLabel "branchNothing_Nothing" test9]

main = defaultMain tests
