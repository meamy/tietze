module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.String

-----------------------------------------------------------------------------------------
-- isSubstrAt

test1 = TestCase (assertEqual "isSubstrAt permits empty substrings (1/2)."
                              (Just 0 :: Maybe Int)
                              ("" `isSubstrAt` ""))

test2 = TestCase (assertEqual "isSubstrAt permits empty substrings (2/2)."
                              (Just 0 :: Maybe Int)
                              ("" `isSubstrAt` "abcdefghijklmnop1234567890"))

test3 = TestCase (assertEqual "isSubstrAt handles empty strings properly."
                              Nothing
                              ("abcdefghijklmnop1234567890" `isSubstrAt` ""))

test4 = TestCase (assertEqual "isSubstrAt rejects subs which are not substrings."
                              Nothing
                              (" " `isSubstrAt` "abcdefghijklmnop1234567890"))

test5 = TestCase (assertEqual "isSubstrAt permits proper prefixes."
                              (Just 0 :: Maybe Int)
                              ("abc" `isSubstrAt` "abcdefghijklmnop1234567890"))

test6 = TestCase (assertEqual "isSubstrAt permits non-prefix substrings (1/2)."
                              (Just 3 :: Maybe Int)
                              ("def" `isSubstrAt` "abcdefghijklmnop1234567890"))

test7 = TestCase (assertEqual "isSubstrAt permits non-prefix substrings (2/2)."
                              (Just 9 :: Maybe Int)
                              ("jklmn" `isSubstrAt` "abcdefghijklmnop1234567890"))

test8 = TestCase (assertEqual "isSubstrAt rejects subs which are partial substrings."
                              Nothing
                              ("8901" `isSubstrAt` "abcdefghijklmnop1234567890"))

-----------------------------------------------------------------------------------------
-- isSubstrOf

test9 = TestCase (assertBool "isSubstrOf permits empty substrings (1/2)."
                             ("" `isSubstrOf` ""))

test10 = TestCase (assertBool "isSubstrOf permits empty substrings (2/2)."
                              ("" `isSubstrOf` "abcdefghijklmnop1234567890"))

test11 = TestCase (assertBool "isSubstrOf handles empty strings properly."
                              (not ("abcdefghijklmnop1234567890" `isSubstrOf` "")))

test12 = TestCase (assertBool "isSubstrOf rejects subs which are not substrings."
                              (not (" " `isSubstrOf` "abcdefghijklmnop1234567890")))

test13 = TestCase (assertBool "isSubstrOf permits proper prefixes."
                              ("abc" `isSubstrOf` "abcdefghijklmnop1234567890"))

test14 = TestCase (assertBool "isSubstrOf permits non-prefix substrings (1/2)."
                              ("def" `isSubstrOf` "abcdefghijklmnop1234567890"))

test15 = TestCase (assertBool "isSubstrOf permits non-prefix substrings (2/2)."
                              ("jklmn" `isSubstrOf` "abcdefghijklmnop1234567890"))

test16 = TestCase (assertBool "isSubstrOf rejects subs which are partial substrings."
                              (not ("8901" `isSubstrOf` "abcdefghijklmnop1234567890")))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "isSubstrAt_BothEmpty" test1,
                                     TestLabel "isSubstrAt_EmptySubstr" test2,
                                     TestLabel "isSubstrAt_EmptyStr" test3,
                                     TestLabel "isSubstrAt_NotSubstr" test4,
                                     TestLabel "isSubstrAt_SubIsPrefix" test5,
                                     TestLabel "isSubstrAt_SubAt3" test6,
                                     TestLabel "isSubstrAt_SubAt9" test7,
                                     TestLabel "isSubstrAt_PartialMatch" test8,
                                     TestLabel "isSubstrOf_BothEmpty" test9,
                                     TestLabel "isSubstrOf_EmptySubstr" test10,
                                     TestLabel "isSubstrOf_EmptyStr" test11,
                                     TestLabel "isSubstrOf_NotSubstr" test12,
                                     TestLabel "isSubstrOf_SubIsPrefix" test13,
                                     TestLabel "isSubstrOf_SubAt3" test14,
                                     TestLabel "isSubstrOf_SubAt9" test16,
                                     TestLabel "isSubstrOf_PartialMatch" test16]

main = defaultMain tests
