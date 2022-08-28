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
                               (maybeApply (Just 101) (\n -> (show n))))

test2 = TestCase (assertEqual "Can use maybeApply to convert an integer to a string."
                               (Just 1 :: Maybe Int)
                               (maybeApply (Just True) (\b -> if b then 1 else 0)))

test3 = TestCase (assertEqual "Can use maybeApply can handle Nothing (1/2)."
                               Nothing
                               (maybeApply nothingAsInt (\n -> (show n))))

test4 = TestCase (assertEqual "Can use maybeApply can handle Nothing (2/2)."
                               Nothing
                               (maybeApply nothingAsBool (\b -> if b then 1 else 0)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "maybeApply_JustIntToJustString" test1,
                                     TestLabel "maybeApply_JustBoolToJustInt" test2,
                                     TestLabel "maybeApply_NothingAsMaybeInt" test3,
                                     TestLabel "maybeApply_NothingAsMaybeBool" test4]

main = defaultMain tests
