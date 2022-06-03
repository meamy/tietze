module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Either
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

test5 = TestCase (assertEqual "Tests that a bad semantic value is rejected (1/2)."
                              (Left (Right (InvalidGenSem 8 "123 45")))
                              (parseGenerator (doFailedParse "123 45") "  abc :=  512"))

test6 = TestCase (assertEqual "Tests that a good semantic value is accepted (1/2)."
                              (Right ("abc", Just 512))
                              (parseGenerator readParse "  abc :=  512"))

test7 = TestCase (assertEqual "Tests that a good semantic value is accepted (1/2)."
                              (Right ("abc", Just 4321))
                              (parseGenerator readParse "  abc :=  4321 "))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseGenerator_BadId" test1,
                                     TestLabel "parseGenerator_OnlyId" test2,
                                     TestLabel "parseGenerator_IdNoSemEq" test3,
                                     TestLabel "parseGenerator_BadSemOne" test4,
                                     TestLabel "parseGenerator_BadSemTwo" test5,
                                     TestLabel "parseGenerator_GoodSemOne" test6,
                                     TestLabel "parseGenerator_GoodSemTwo" test7]

main = defaultMain tests
