module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Parse.Circuits

-----------------------------------------------------------------------------------------
-- parseParam

test1 = TestCase (assertEqual "parseParam rejects empty strings."
                              Nothing
                              (parseParam ""))

test2 = TestCase (assertEqual "parseParam rejects only opening brackets."
                              Nothing
                              (parseParam "["))

test3 = TestCase (assertEqual "parseParam rejects strs without closing brackets (1/2)."
                              Nothing
                              (parseParam "[10"))

test4 = TestCase (assertEqual "parseParam rejects strs without closing brackets (2/2)."
                              Nothing
                              (parseParam "[10abc"))

test5 = TestCase (assertEqual "parseParam rejects incorrect closing brackets."
                              Nothing
                              (parseParam "[10[abc"))

test6 = TestCase (assertEqual "parseParam rejects incorrect symbols inside brackets."
                              Nothing
                              (parseParam "[10abc]"))

test7 = TestCase (assertEqual "parseParam rejects negative numbers inside brackets."
                              Nothing
                              (parseParam "[-10]"))

test8 = TestCase (assertEqual "parseParam supports a single parameter."
                              (Just (10, "") :: Maybe (Int, String))
                              (parseParam "[10]"))

test9 = TestCase (assertEqual "parseParam handles post parameter data (1/2)."
                              (Just (10, "abc") :: Maybe (Int, String))
                              (parseParam "[10]abc"))

test10 = TestCase (assertEqual "parseParam handles post parameter data (1/2)."
                               (Just (20, "abc") :: Maybe (Int, String))
                               (parseParam "[20]abc"))

-----------------------------------------------------------------------------------------
-- parseParams

test11 = TestCase (assertEqual "parseParams rejects empty strings."
                              ([], "")
                              (parseParams ""))

test12 = TestCase (assertEqual "parseParams reject partial parameter lists."
                              ([], "[1")
                              (parseParams "[1"))

test13 = TestCase (assertEqual "parseParams can parse a single parameter."
                              ([1], "")
                              (parseParams "[1]"))

test14 = TestCase (assertEqual "parseParams can parse two parameter."
                               ([1, 2], "")
                               (parseParams "[1][2]"))

test15 = TestCase (assertEqual "parseParams can parse three parameter."
                               ([1, 2, 3], "")
                               (parseParams "[1][2][3]"))

test16 = TestCase (assertEqual "parseParams can parse four parameter."
                               ([1, 2, 3, 4], "")
                               (parseParams "[1][2][3][4]"))

test17 = TestCase (assertEqual "parseParams can handle unparsed postfixes."
                               ([1, 2, 3, 4], "abdfsfa[12")
                               (parseParams "[1][2][3][4]abdfsfa[12"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseParam_EmptyString" test1,
                                     TestLabel "parseParam_OnlyOpenBracket" test2,
                                     TestLabel "parseParam_NoClosingBracket_Test0" test3,
                                     TestLabel "parseParam_NoClosingBracket_Test1" test4,
                                     TestLabel "parseParam_WrongClosingBracket" test5,
                                     TestLabel "parseParam_BadNumber" test6,
                                     TestLabel "parseParam_NegativeNumber" test7,
                                     TestLabel "parseParam_Valid_Test0" test8,
                                     TestLabel "parseParam_Valid_Test1" test9,
                                     TestLabel "parseParam_Valid_Test2" test10,
                                     TestLabel "parseParams_EmptyString" test11,
                                     TestLabel "parseParams_NotFullParam" test12,
                                     TestLabel "parseParams_OneParam" test13,
                                     TestLabel "parseParams_TwoParams" test14,
                                     TestLabel "parseParams_ThreeParams" test15,
                                     TestLabel "parseParams_FourParams" test16,
                                     TestLabel "parseParams_PostString" test17]

main = defaultMain tests
