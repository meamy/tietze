module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Either
import DyadicRewrite.Generators.Semantics
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.GeneratorFile

-----------------------------------------------------------------------------------------
-- parseGenerator

doFailedParse :: String -> String -> Either String String
doFailedParse msg _ = Left msg

readParse :: String -> Either String Int
readParse str = (Right (read str :: Int))

test1 = TestCase (assertEqual "Tests that bad generator ID's are rejected."
                              (Left (Right InvalidGenName))
                              (parseGenerator (doFailedParse "") "1abc"))

test2 = TestCase (assertEqual "Tests that a lone generator can be read."
                              (Right ("abc", Nothing))
                              (parseGenerator (doFailedParse "") "  abc  "))

test3 = TestCase (assertEqual "Tests that a lone generator can be read."
                              (Left (Left (UnexpectedSymbol 7)))
                              (parseGenerator (doFailedParse "") "  abc  abc"))

test4 = TestCase (assertEqual "Tests that a bad semantic value is rejected (1/2)."
                              (Left (Right (InvalidGenSem 9 "abc")))
                              (parseGenerator (doFailedParse "abc") "  abc  :=  512"))

test5 = TestCase (assertEqual "Tests that a bad semantic value is rejected (2/2)."
                              (Left (Right (InvalidGenSem 8 "123 45")))
                              (parseGenerator (doFailedParse "123 45") "  abc :=  512"))

test6 = TestCase (assertEqual "Tests that a good semantic value is accepted (1/2)."
                              (Right ("abc", Just 512))
                              (parseGenerator readParse "  abc :=  512"))

test7 = TestCase (assertEqual "Tests that a good semantic value is accepted (2/2)."
                              (Right ("abc", Just 4321))
                              (parseGenerator readParse "  abc :=  4321 "))

-----------------------------------------------------------------------------------------
-- updateGenerators

copyStr :: String -> Either String String
copyStr semStr = Right semStr

gen1 :: (String, Maybe String)
gen1 = ("xyx", Just "qwerty")

gen2 :: (String, Maybe String)
gen2 = ("x123", Just "lmnop")

emptyDict :: GenDict String
emptyDict = empty

dupDict = addGen emptyDict ("abc", Nothing)
dict1 = addGen emptyDict gen1
dict2 = addGen dict1 gen2

test8 = TestCase (assertEqual "Tests that updateGenerators propogates errors (1/2)."
                              (Left (Right InvalidGenName))
                              (updateGenerators (doFailedParse "") emptyDict "1abc"))

test9 = TestCase (assertEqual "Tests that updateGenerators propogates errors (2/2)."
                              (Left (Right (InvalidGenSem 5 "1")))
                              (updateGenerators (doFailedParse "1") emptyDict "abc:=1"))

test10 = TestCase (assertEqual "Tests that updateGenerators detects duplicate names."
                               (Left (Right (DuplicateGenName "abc")))
                               (updateGenerators copyStr dupDict "abc:=1"))

test11 = TestCase (assertEqual "Tests that updateGenerators adds new generators."
                               (Just "1" :: Maybe String)
                               semv)
         where semv = case (updateGenerators copyStr emptyDict "abc:=1") of
                          Left err   -> Nothing
                          Right dict -> interpretGen dict "abc"

test12 = TestCase (assertEqual "Tests that updateGenerators preserves generators (1/3)."
                               (snd gen1)
                               semv)
         where semv = case (updateGenerators copyStr dict2 "abc:=1") of
                          Left err   -> Nothing
                          Right dict -> interpretGen dict (fst gen1)

test13 = TestCase (assertEqual "Tests that updateGenerators preserves generators (2/3)."
                               (snd gen2)
                               semv)
         where semv = case (updateGenerators copyStr dict2 "abc:=1") of
                          Left err   -> Nothing
                          Right dict -> interpretGen dict (fst gen2)

test14 = TestCase (assertEqual "Tests that updateGenerators preserves generators (3/3)."
                               (Just "1" :: Maybe String)
                               semv)
         where semv = case (updateGenerators copyStr dict2 "abc:=1") of
                          Left err   -> Nothing
                          Right dict -> interpretGen dict "abc"

test15 = TestCase (assertEqual "Tests that updateGenerators supports no semantics."
                               Nothing
                               semv)
         where semv = case (updateGenerators copyStr emptyDict "abc") of
                          Left err   -> Nothing
                          Right dict -> interpretGen dict "abc"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseGenerator_BadId" test1,
                                     TestLabel "parseGenerator_OnlyId" test2,
                                     TestLabel "parseGenerator_IdNoSemEq" test3,
                                     TestLabel "parseGenerator_BadSemOne" test4,
                                     TestLabel "parseGenerator_BadSemTwo" test5,
                                     TestLabel "parseGenerator_GoodSemOne" test6,
                                     TestLabel "parseGenerator_GoodSemTwo" test7,
                                     TestLabel "updateGenerators_PropogateOne" test8,
                                     TestLabel "updateGenerators_PropogateTwo" test9,
                                     TestLabel "updateGenerators_DetectsDupName" test10,
                                     TestLabel "updateGenerators_AddNewGen" test11,
                                     TestLabel "updateGenerators_UpdateGens_0" test12,
                                     TestLabel "updateGenerators_UpdateGens_1" test13,
                                     TestLabel "updateGenerators_UpdateGens_2" test14,
                                     TestLabel "updateGenerators_NoSemantics" test15]

main = defaultMain tests
