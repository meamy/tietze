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
-- Symbol: order

sym1 = Symbol "xyz" [0, 0]
sym2 = Symbol "aaaaa" [0, 0]
sym3 = Symbol "aaaaa" [0]

test7 = TestCase (assertEqual "Display defaults to comparison by strings (GT)."
                              GT
                              (compare sym1 sym2))

test8 = TestCase (assertEqual "Display defaults to comparison by strings (LT)."
                              LT
                              (compare sym2 sym1))

test9 = TestCase (assertEqual "Display defaults to comparison by parameters (GT)."
                              GT
                              (compare sym2 sym3))

test10 = TestCase (assertEqual "Display defaults to comparison by parameters (LT)."
                               LT
                               (compare sym3 sym2))

test11 = TestCase (assertEqual "Display defaults to comparison by parameters (EQ)."
                               EQ
                               (compare sym3 sym3))

-----------------------------------------------------------------------------------------
-- Symbol: toSymbol

test12 = TestCase (assertEqual "Display a symbol with zero parameters (1/2)."
                               (Symbol "name1" [])
                               (toSymbol "name1"))

test13 = TestCase (assertEqual "Display a symbol with zero parameters (2/2)."
                               (Symbol "name2" [])
                               (toSymbol "name2"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Symbol_Display_NoParamsOne" test1,
                                     TestLabel "Symbol_Display_NoParamsTwo" test2,
                                     TestLabel "Symbol_Display_OneParamOne" test3,
                                     TestLabel "Symbol_Display_OneParamTwo" test4,
                                     TestLabel "Symbol_Display_TwoParamsOne" test5,
                                     TestLabel "Symbol_Display_TwoParamsTwo" test6,
                                     TestLabel "Symbol_Ord_ByStr_GT" test7,
                                     TestLabel "Symbol_Ord_ByStr_LT" test8,
                                     TestLabel "Symbol_Ord_ByParams_GT" test9,
                                     TestLabel "Symbol_Ord_ByParams_LT" test10,
                                     TestLabel "Symbol_Ord_ByParams_EQ" test11,
                                     TestLabel "toSymbol_1" test12,
                                     TestLabel "toSymbol_2" test13]

main = defaultMain tests
