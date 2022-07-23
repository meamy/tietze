module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common

-----------------------------------------------------------------------------------------
-- Symbol: display

test1 = TestCase (assertEqual "Display a symbol with zero parameters (1/2)."
                              "name1"
                              (display (Symbol "name1" [])))

test2 = TestCase (assertEqual "Display a symbol with zero parameters (2/2)."
                              "name2"
                              (display (Symbol "name2" [])))

test3 = TestCase (assertEqual "Display a symbol with one parameter (1/2)."
                              "name1[1]"
                              (display (Symbol "name1" [1])))

test4 = TestCase (assertEqual "Display a symbol with one parameter (2/2)."
                              "name2[2]"
                              (display (Symbol "name2" [2])))

test5 = TestCase (assertEqual "Display a symbol with two parameter (1/2)."
                              "name1[1][2]"
                              (display (Symbol "name1" [1, 2])))

test6 = TestCase (assertEqual "Display a symbol with two parameter (2/2)."
                              "name2[2][3]"
                              (display (Symbol "name2" [2, 3])))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Symbol_Display_NoParamsOne" test1,
                                     TestLabel "Symbol_Display_NoParamsTwo" test2,
                                     TestLabel "Symbol_Display_OneParamOne" test3,
                                     TestLabel "Symbol_Display_OneParamTwo" test4,
                                     TestLabel "Symbol_Display_TwoParamsOne" test5,
                                     TestLabel "Symbol_Display_TwoParamsTwo" test6]

main = defaultMain tests
