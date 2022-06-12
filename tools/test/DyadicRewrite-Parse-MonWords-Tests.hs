module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Parse.MonWords

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
-- parseSymbol

test18 = TestCase (assertEqual "parseSymbol rejects empty strings."
                               Nothing
                               (parseSymbol ""))

test19 = TestCase (assertEqual "parseSymbol rejects bad ID's."
                               Nothing
                               (parseSymbol "1abc[1][2]"))

test20 = TestCase (assertEqual "parseSymbol rejects on missing ID's."
                               Nothing
                               (parseSymbol "[2]"))

test21 = TestCase (assertEqual "parseSymbol parses unparameterized symbols."
                               (Just ((Symbol "abc" []), "") :: Maybe (Symbol, String))
                               (parseSymbol "abc"))

test22 = TestCase (assertEqual "parseSymbol parses unparameterized symbols with postfixes."
                               (Just ((Symbol "abc" []), ".") :: Maybe (Symbol, String))
                               (parseSymbol "abc."))

test23 = TestCase (assertEqual "parseSymbol parses single parameter symbols."
                               (Just ((Symbol "abc" [1]), "") :: Maybe (Symbol, String))
                               (parseSymbol "abc[1]"))

test24 = TestCase (assertEqual "parseSymbol parses two parameter symbols."
                               (Just ((Symbol "abc" [1, 2]), "") :: Maybe (Symbol, String))
                               (parseSymbol "abc[1][2]"))

test25 = TestCase (assertEqual "parseSymbol parses three parameter symbols."
                               (Just ((Symbol "abc" [1, 2, 3]), "") :: Maybe (Symbol, String))
                               (parseSymbol "abc[1][2][3]"))

test26 = TestCase (assertEqual "parseSymbol parses parameterized symbols with postfixes."
                               (Just ((Symbol "abc" [1, 2, 3]), "[4.") :: Maybe (Symbol, String))
                               (parseSymbol "abc[1][2][3][4."))

-----------------------------------------------------------------------------------------
-- parseNonEmptyMonWord

word1 :: MonWord
word1 = [(Symbol "abc" [])]

word2 :: MonWord
word2 = [(Symbol "abc" []), (Symbol "def" [])]

word3 :: MonWord
word3 = [(Symbol "abc" []), (Symbol "def" []), (Symbol "ghi" [])]

word4 :: MonWord
word4 = [(Symbol "abc" []), (Symbol "def" [1]), (Symbol "ghi" [1, 2])]

test27 = TestCase (assertEqual "parseNonEmptyMonWord rejects empty strings."
                               Nothing
                               (parseNonEmptyMonWord ""))

test28 = TestCase (assertEqual "parseNonEmptyMonWord rejects bad identifiers."
                               Nothing
                               (parseNonEmptyMonWord "1abc"))

test29 = TestCase (assertEqual "parseNonEmptyMonWord accepts single symbol words."
                               (Just (word1, "") :: Maybe (MonWord, String))
                               (parseNonEmptyMonWord "abc"))

test30 = TestCase (assertEqual "parseNonEmptyMonWord rejects incomplete two symbol words."
                               Nothing
                               (parseNonEmptyMonWord "abc."))

test31 = TestCase (assertEqual "parseMonWord accepts two symbol words."
                               (Just (word2, "") :: Maybe (MonWord, String))
                               (parseNonEmptyMonWord "abc.def"))

test32 = TestCase (assertEqual "parseNonEmptyMonWord rejects partial three symbol words."
                               Nothing
                               (parseNonEmptyMonWord "abc.def."))

test33 = TestCase (assertEqual "parseNonEmptyMonWord accepts three symbol words."
                               (Just (word3, "") :: Maybe (MonWord, String))
                               (parseNonEmptyMonWord "abc.def.ghi"))

test34 = TestCase (assertEqual "parseNonEmptyMonWord accepts parameterized words."
                               (Just (word4, "") :: Maybe (MonWord, String))
                               (parseNonEmptyMonWord "abc.def[1].ghi[1][2]"))

test35 = TestCase (assertEqual "parseNonEmptyMonWord accepts postfixes."
                               (Just (word4, " .asd") :: Maybe (MonWord, String))
                               (parseNonEmptyMonWord "abc.def[1].ghi[1][2] .asd"))

test36 = TestCase (assertEqual "parseNonEmptyMonWord requires that strings terminate."
                               Nothing
                               (parseNonEmptyMonWord "abc.def[1].ghi[1][2]asd"))

-----------------------------------------------------------------------------------------
-- parseMonWord

test38 = TestCase (assertEqual "parseMonWord supports empty strings."
                               (Just (word4, " .asd") :: Maybe (MonWord, String))
                               (parseMonWord "abc.def[1].ghi[1][2] .asd"))

test39 = TestCase (assertEqual "parseMonWord supports non-empty strings."
                               (Just ([], ".asd") :: Maybe (MonWord, String))
                               (parseMonWord "ε.asd"))

-----------------------------------------------------------------------------------------
-- parseLineAsMonWord

test40 = TestCase (assertEqual "parseLineAsMonWord supports empty lines."
                               Nothing
                               (parseLineAsMonWord ""))

test41 = TestCase (assertEqual "parseLineAsMonWord supports non-word lines."
                               Nothing
                               (parseLineAsMonWord "    @name rel1    -- comment"))

test42 = TestCase (assertEqual "parseLineAsMonWord supports empty words."
                               (Just [] :: Maybe MonWord)
                               (parseLineAsMonWord "    ε    -- comment"))

test43 = TestCase (assertEqual "parseLineAsMonWord supports non-empty words."
                               (Just word2 :: Maybe MonWord)
                               (parseLineAsMonWord "    abc.def    -- comment"))

test44 = TestCase (assertEqual "parseLineAsMonWord supports trailing symbols."
                               Nothing
                               (parseLineAsMonWord "    abc.def  xyz  -- comment"))

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
                                     TestLabel "parseParams_PostString" test17,
                                     TestLabel "parseSymbol_EmptyString" test18,
                                     TestLabel "parseSymbol_BadID" test19,
                                     TestLabel "parseSymbol_NoID" test20,
                                     TestLabel "parseSymbol_NoParams" test21,
                                     TestLabel "parseSymbol_NoParamsPostString" test22,
                                     TestLabel "parseSymbol_OneParam" test23,
                                     TestLabel "parseSymbol_TwoParams" test24,
                                     TestLabel "parseSymbol_ThreeParams" test25,
                                     TestLabel "parseSymbol_ParamsPostString" test26,
                                     TestLabel "parseNonEmptyMonWord_EmptyString" test27,
                                     TestLabel "parseNonEmptyMonWord_BadID" test28,
                                     TestLabel "parseNonEmptyMonWord_OneSymbol" test29,
                                     TestLabel "parseNonEmptyMonWord_OneSymbolBadDot" test30,
                                     TestLabel "parseNonEmptyMonWord_TwoSymbols" test31,
                                     TestLabel "parseNonEmptyMonWord_TwoSymbolsBadDot" test32,
                                     TestLabel "parseNonEmptyMonWord_ThreeSymbols" test33,
                                     TestLabel "parseNonEmptyMonWord_ParamSymbols" test34,
                                     TestLabel "parseNonEmptyMonWord_PostString" test35,
                                     TestLabel "parseNonEmptyMonWord_MissingSpace" test36,
                                     TestLabel "parseMonWord_NonEmpty" test38,
                                     TestLabel "parseMonWord_Empty" test39,
                                     TestLabel "parseLineAsMonWord_EmptyLine" test40,
                                     TestLabel "parseLineAsMonWord_NonWordLine" test41,
                                     TestLabel "parseLineAsMonWord_EmptyWord" test42,
                                     TestLabel "parseLineAsMonWord_NonEmptyWord" test43,
                                     TestLabel "parseLineAsMonWord_TrailingSymbols" test44]

main = defaultMain tests
