module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Common

-----------------------------------------------------------------------------------------
-- Gate: show

test1 = TestCase (assertEqual "Show a gate with zero parameters (1/2)."
                              "name1"
                              (show (Gate "name1" [])))

test2 = TestCase (assertEqual "Show a gate with zero parameters (2/2)."
                              "name2"
                              (show (Gate "name2" [])))

test3 = TestCase (assertEqual "Show a gate with one parameter (1/2)."
                              "name1[1]"
                              (show (Gate "name1" [1])))

test4 = TestCase (assertEqual "Show a gate with one parameter (2/2)."
                              "name2[2]"
                              (show (Gate "name2" [2])))

test5 = TestCase (assertEqual "Show a gate with two parameter (1/2)."
                              "name1[1][2]"
                              (show (Gate "name1" [1, 2])))

test6 = TestCase (assertEqual "Show a gate with two parameter (2/2)."
                              "name2[2][3]"
                              (show (Gate "name2" [2, 3])))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Gate_Show_NoParamsOne" test1,
                                     TestLabel "Gate_Show_NoParamsTwo" test2,
                                     TestLabel "Gate_Show_OneParamOne" test3,
                                     TestLabel "Gate_Show_OneParamTwo" test4,
                                     TestLabel "Gate_Show_TwoParamsOne" test5,
                                     TestLabel "Gate_Show_TwoParamsTwo" test6]

main = defaultMain tests
