module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common

-----------------------------------------------------------------------------------------
-- Symbol: show

test1 = TestCase (assertEqual "Show a symbol with zero parameters (1/2)."
                              "name1"
                              (show (Symbol "name1" [])))

test2 = TestCase (assertEqual "Show a symbol with zero parameters (2/2)."
                              "name2"
                              (show (Symbol "name2" [])))

test3 = TestCase (assertEqual "Show a symbol with one parameter (1/2)."
                              "name1[1]"
                              (show (Symbol "name1" [1])))

test4 = TestCase (assertEqual "Show a symbol with one parameter (2/2)."
                              "name2[2]"
                              (show (Symbol "name2" [2])))

test5 = TestCase (assertEqual "Show a symbol with two parameter (1/2)."
                              "name1[1][2]"
                              (show (Symbol "name1" [1, 2])))

test6 = TestCase (assertEqual "Show a symbol with two parameter (2/2)."
                              "name2[2][3]"
                              (show (Symbol "name2" [2, 3])))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Symbol_Show_NoParamsOne" test1,
                                     TestLabel "Symbol_Show_NoParamsTwo" test2,
                                     TestLabel "Symbol_Show_OneParamOne" test3,
                                     TestLabel "Symbol_Show_OneParamTwo" test4,
                                     TestLabel "Symbol_Show_TwoParamsOne" test5,
                                     TestLabel "Symbol_Show_TwoParamsTwo" test6]

main = defaultMain tests
