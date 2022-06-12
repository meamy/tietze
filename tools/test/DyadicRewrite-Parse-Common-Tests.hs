module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- splitAtFirst

test1 = TestCase (assertEqual "Splitting an empty string returns tuple of empty strings."
                              ("","")
                              (splitAtFirst (\x -> True) ""))

test2 = TestCase (assertEqual "Splitting an empty string returns tuple of empty strings."
                              ("","")
                              (splitAtFirst (\x -> False) ""))

test3 = TestCase (assertEqual "Splitting with true pred consumes entire string."
                              ("qwertyuiop1234567890", "")
                              (splitAtFirst (\x -> True) "qwertyuiop1234567890"))

test4 = TestCase (assertEqual "Splitting with true pred consumes entire string."
                              ("", "qwertyuiop1234567890")
                              (splitAtFirst (\x -> False) "qwertyuiop1234567890"))

test5 = TestCase (assertEqual "Splitting with non-trivial pred with a match."
                              ("abcabc", "123")
                              (splitAtFirst (\x -> x `elem` "abcabc") "abcabc123"))

test6 = TestCase (assertEqual "Splitting with non-trivial pred without a match."
                              ("", "abcabc123")
                              (splitAtFirst (\x -> x `elem` "123") "abcabc123"))

-----------------------------------------------------------------------------------------
-- parseNat

test7 = TestCase (assertEqual "parseNat cannot parse an empty string."
                              Nothing
                              (parseNat ""))

test8 = TestCase (assertEqual "parseNat cannot parse an alphabetical string."
                              Nothing
                              (parseNat "abc"))

test9 = TestCase (assertEqual "parseNat can parse positive integers."
                              (Just (123, "") :: Maybe (Int, String))
                              (parseNat "123"))

test10 = TestCase (assertEqual "parseNat will only parse decimal strings."
                               (Just (123, "abc") :: Maybe (Int, String))
                               (parseNat "123abc"))

test11 = TestCase (assertEqual "parseNat stops after first non-numeric character."
                               Nothing
                               (parseNat "abc123"))

test12 = TestCase (assertEqual "parseNat cannot parse negative integers."
                               Nothing
                               (parseNat "-123"))

test13 = TestCase (assertEqual "parseNat handles multiple zeros."
                               (Just (0, "abc") :: Maybe (Int, String))
                               (parseNat "00000abc"))

-----------------------------------------------------------------------------------------
-- parseInt

test14 = TestCase (assertEqual "parseInt cannot parse an empty string."
                               Nothing
                               (parseInt ""))

test15 = TestCase (assertEqual "parseInt cannot parse an alphabetical string."
                               Nothing
                               (parseInt "abc"))

test16 = TestCase (assertEqual "parseInt can parse positive integers."
                               (Just (123, "") :: Maybe (Int, String))
                               (parseInt "123"))

test17 = TestCase (assertEqual "parseInt will only parse decimal strings."
                                (Just (123, "abc") :: Maybe (Int, String))
                                (parseInt "123abc"))

test18 = TestCase (assertEqual "parseInt stops after first non-numeric character."
                                Nothing
                                (parseInt "abc123"))

test19 = TestCase (assertEqual "parseInt parses negative integers."
                               (Just (-123, "") :: Maybe (Int, String))
                               (parseInt "-123"))

test20 = TestCase (assertEqual "parseInt handles multiple zeros."
                               (Just (0, "abc") :: Maybe (Int, String))
                               (parseInt "00000abc"))

test21 = TestCase (assertEqual "parseInt handles negative zeros."
                               (Just (0, "") :: Maybe (Int, String))
                               (parseInt "-0"))

test22 = TestCase (assertEqual "parseInt stops after first non-numeric character (neg)."
                               (Just (-123, "abc") :: Maybe (Int, String))
                               (parseInt "-123abc"))

test23 = TestCase (assertEqual "parseInt rejects double negation."
                               Nothing
                               (parseInt "--123"))

test24 = TestCase (assertEqual "parseInt leading rejects dashes outside of numbers."
                               Nothing
                               (parseInt "-abc"))

-----------------------------------------------------------------------------------------
-- isSpacing

test25 = TestCase (assertBool "isSpacing returns true on a space." 
                              (isSpacing ' '))

test26 = TestCase (assertBool "isSpacing returns true on a tab." 
                              (isSpacing '\t'))

test27 = TestCase (assertBool "isSpacing returns false on a newline." 
                              (not (isSpacing '\n')))

test28 = TestCase (assertBool "isSpacing returns false on a return." 
                              (not (isSpacing '\r')))

test29 = TestCase (assertBool "isSpacing returns false on a letter." 
                              (not (isSpacing 'a')))

test30 = TestCase (assertBool "isSpacing returns false on a number." 
                              (not (isSpacing '4')))

test31 = TestCase (assertBool "isSpacing returns false on a non-numeric symbol." 
                              (not (isSpacing '-')))

-----------------------------------------------------------------------------------------
-- trimSpacing

test32 = TestCase (assertEqual "A space without strings is unchanged and not trimmed."
                               (False, "abc")
                               (trimSpacing "abc"))

test33 = TestCase (assertEqual "A string with newlines is unchanged and not trimmed."
                               (False, "\nabc")
                               (trimSpacing "\nabc"))

test34 = TestCase (assertEqual "A string with a leading space is trimmed."
                               (True, "abc")
                               (trimSpacing " abc"))

test35 = TestCase (assertEqual "A string with two leading spaces is trimmed."
                               (True, "abc")
                               (trimSpacing "  abc"))

test36 = TestCase (assertEqual "A string with a leading tab is trimmed."
                               (True, "abc")
                               (trimSpacing "\tabc"))

test37 = TestCase (assertEqual "A string with two leading tabs is trimmed."
                               (True, "abc")
                               (trimSpacing "\t\tabc"))

test38 = TestCase (assertEqual "A space with mixed tabs and spaces is trimmed."
                               (True, "abcdefg")
                               (trimSpacing "   \t\t  \t\t\t\t \t abcdefg"))

test39 = TestCase (assertEqual "Intermediate spaces are not changed."
                               (False, "a   \t \t   abcdefg")
                               (trimSpacing "a   \t \t   abcdefg"))

-----------------------------------------------------------------------------------------
-- parseNonEmpty

test40 = TestCase (assertEqual "parseNonEmpty can return nothing."
                               Nothing
                               (parseNonEmpty (\x -> False) "qwertyuiop1234567890"))

test41 = TestCase (assertEqual "parseNonEmpty can return just a result."
                               (Just ("abcabc", "123") :: Maybe (String, String))
                               (parseNonEmpty (\x -> x `elem` "abcabc") "abcabc123"))

-----------------------------------------------------------------------------------------
-- parseId

test42 = TestCase (assertEqual "parseId returns nothing on unsupported characters (1/2)."
                               Nothing
                               (parseId "-123"))

test43 = TestCase (assertEqual "parseId returns nothing on unsupported characters (2/2)."
                               Nothing
                               (parseId " abc"))

test44 = TestCase (assertEqual "parseId returns nothing on leading digit."
                               Nothing
                               (parseId "1abc"))

test45 = TestCase (assertEqual "parseId can parse a valid identifier."
                               (Just ("abc_123", "-12 456") :: Maybe (String, String))
                               (parseId "abc_123-12 456"))

-----------------------------------------------------------------------------------------
-- parseSep

test46 = TestCase (assertEqual "parseSep can work on empty inputs."
                               (Just "" :: Maybe String)
                               (parseSep "" ""))

test47 = TestCase (assertEqual "parseSep can work with an empty string."
                              Nothing
                               (parseSep "abc" ""))

test48 = TestCase (assertEqual "parseSep can work with an empty separator."
                               (Just "abc" :: Maybe String)
                               (parseSep "" "abc"))

test49 = TestCase (assertEqual "parseSep can work on matching separator."
                               (Just "123abc123" :: Maybe String)
                               (parseSep "abc" "abc123abc123"))

test50 = TestCase (assertEqual "parseSep can work with whitespace on the lhs."
                               (Just "123abc123" :: Maybe String)
                               (parseSep "abc" "   abc123abc123"))

test51 = TestCase (assertEqual "parseSep can work with whitespace on the rhs."
                               (Just "      123abc123" :: Maybe String)
                               (parseSep "abc" "abc      123abc123"))

test52 = TestCase (assertEqual "parseSep can work with whitespace on both sides."
                               (Just "      123abc123" :: Maybe String)
                               (parseSep "abc" "   abc      123abc123"))

-----------------------------------------------------------------------------------------
-- parseFromSeps

test53 = TestCase (assertEqual "parseFromSeps can work with zero separators."
                               Nothing
                               (parseFromSeps [] "abc"))

test54 = TestCase (assertEqual "parseFromSeps can fail to match separators."
                               Nothing
                               (parseFromSeps ["123"] "abc"))

test55 = TestCase (assertEqual "parseFromSeps can match separators."
                               (Just ("abc", "123") :: Maybe (String, String))
                               (parseFromSeps ["abc"] "abc123"))

test56 = TestCase (assertEqual "parseFromSeps can search for a matching separator."
                               (Just ("!@", " 123a") :: Maybe (String, String))
                               (parseFromSeps ["abc", "123", "!@", "999"] "   !@ 123a"))

-----------------------------------------------------------------------------------------
-- getErrPos

test57 = TestCase (assertEqual "getErrPos works on empty strings."
                               0
                               (getErrPos "" ""))

test58 = TestCase (assertEqual "getErrPos works on empty unparsed string."
                               6
                               (getErrPos "abcdef" ""))

test59 = TestCase (assertEqual "getErrPos works on full unparsed string."
                               0
                               (getErrPos "abcdef" "abcdef"))

test60 = TestCase (assertEqual "getErrPos works on proper substrings (1/2)."
                               3
                               (getErrPos "abcdefab" "defab"))

test61 = TestCase (assertEqual "getErrPos works on proper substrings (1/2)."
                               6
                               (getErrPos "abcdefab" "ab"))

-----------------------------------------------------------------------------------------
-- branchOnSpacing

test62 = TestCase (assertEqual "bracnhOnSpacing supports empty strings."
                               (Right "Hello" :: Either Int String)
                               (branchOnSpacing "" 1 "Hello"))

test63 = TestCase (assertEqual "bracnhOnSpacing supports spaces."
                               (Right "Hello" :: Either Int String)
                               (branchOnSpacing " " 1 "Hello"))

test64 = TestCase (assertEqual "bracnhOnSpacing supports tabbing."
                               (Right "Hello" :: Either Int String)
                               (branchOnSpacing "\t" 1 "Hello"))

test65 = TestCase (assertEqual "bracnhOnSpacing supports non-spacing."
                               (Left 1 :: Either Int String)
                               (branchOnSpacing "a" 1 "Hello"))

test66 = TestCase (assertEqual "bracnhOnSpacing supports mixed spacing."
                               (Right "Hello" :: Either Int String)
                               (branchOnSpacing "  \t   " 1 "Hello"))

test67 = TestCase (assertEqual "branchOnSpacing detects non-spacing characters later on."
                               (Left 1 :: Either Int String)
                               (branchOnSpacing "  \t   a" 1 "Hello"))

-----------------------------------------------------------------------------------------
-- relToAbsErrPos

test68 = TestCase (assertEqual "relToAbsErrPos works on empty strings."
                               0
                               (relToAbsErrPos "" "" 0))

test69 = TestCase (assertEqual "relToAbsErrPos works on empty unparsed string."
                               6
                               (relToAbsErrPos "abcdef" "" 0))

test70 = TestCase (assertEqual "relToAbsErrPos works on full unparsed string (1/2)."
                               0
                               (relToAbsErrPos "abcdef" "abcdef" 0))

test71 = TestCase (assertEqual "relToAbsErrPos works on full unparsed string (2/2)."
                               2
                               (relToAbsErrPos "abcdef" "abcdef" 2))

test72 = TestCase (assertEqual "relToAbsErrPos works on proper substrings, 0 pos (1/2)."
                               3
                               (relToAbsErrPos "abcdefab" "defab" 0))

test73 = TestCase (assertEqual "relToAbsErrPos works on proper substrings, 0 pos (2/2)."
                               6
                               (relToAbsErrPos "abcdefab" "ab" 0))

test74 = TestCase (assertEqual "relToAbsErrPos works on proper substrings (1/2)."
                               4
                               (relToAbsErrPos "abcdefab" "defab" 1))

test75 = TestCase (assertEqual "relToAbsErrPos works on proper substrings (2/2)."
                               7
                               (relToAbsErrPos "abcdefab" "ab" 1))

-----------------------------------------------------------------------------------------
-- stripComments

test76 = TestCase (assertEqual "stripComments supports the empty string."
                               ""
                               (stripComments ""))

test77 = TestCase (assertEqual "stripComments supports strings without comments."
                               "This - is - a - valid - string ! 1234"
                               (stripComments "This - is - a - valid - string ! 1234"))

test78 = TestCase (assertEqual "stripComments strips comments."
                               "gen := H[1].Z[1].H[1] "
                               (stripComments "gen := H[1].Z[1].H[1] -- This is X[1]"))

-----------------------------------------------------------------------------------------
-- cleanLine

test79 = TestCase (assertEqual "cleanLine supports the empty string."
                               ""
                               (cleanLine ""))

test80 = TestCase (assertEqual "cleanLine supports strings without trimming."
                               "This - is - a - valid - string ! 1234"
                               (cleanLine "This - is - a - valid - string ! 1234"))

test81 = TestCase (assertEqual "cleanLine omits leading spacing."
                               "This - is - a - valid - string"
                               (cleanLine "  \t\t  \t\t This - is - a - valid - string"))

test82 = TestCase (assertEqual "cleanLine omits comments."
                               "This - is - a - valid - string "
                               (cleanLine "This - is - a - valid - string -- xyz abc"))

test83 = TestCase (assertEqual "cleanLine handles comments and spacing (1/2)."
                               "words words words "
                               (cleanLine "\t  words words words -- comments comments"))

test84 = TestCase (assertEqual "cleanLine handles comments and spacing (2/2)."
                               ""
                               (cleanLine "  \t\t   \t\t\t  -- comments comments..."))

-----------------------------------------------------------------------------------------
-- iteOnSpacing

test85 = TestCase (assertEqual "iteOnSpacing supports empty strings."
                               5
                               (iteOnSpacing "" 1 5))

test86 = TestCase (assertEqual "iteOnSpacing supports spaces."
                               5
                               (iteOnSpacing " " 1 5))

test87 = TestCase (assertEqual "iteOnSpacing supports tabbing."
                               5
                               (iteOnSpacing "\t" 1 5))

test88 = TestCase (assertEqual "iteOnSpacing supports non-spacing."
                               1
                               (iteOnSpacing "a" 1 5))

test89 = TestCase (assertEqual "iteOnSpacing supports mixed spacing."
                               5
                               (iteOnSpacing "  \t   " 1 5))

test90 = TestCase (assertEqual "iteOnSpacing detects non-spacing characters later on."
                               1
                               (iteOnSpacing "  \t   a" 1 5))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "splitAtFirst_TruePred_EmptyStr" test1,
                                     TestLabel "splitAtFirst_FalsePred_EmptyStr" test2,
                                     TestLabel "splitAtFirst_TruePred" test3,
                                     TestLabel "splitAtFirst_FalsePred" test4,
                                     TestLabel "splitAtFirst_MatchingPred" test5,
                                     TestLabel "splitAtFirst_NonMatchingPred" test6,
                                     TestLabel "parseNat_EmptyString" test7,
                                     TestLabel "parseNat_abc" test8,
                                     TestLabel "parseNat_123" test9,
                                     TestLabel "parseNat_123abc" test10,
                                     TestLabel "parseNat_abc123" test11,
                                     TestLabel "parseNat_-123" test12,
                                     TestLabel "parseNat_00000abc" test13,
                                     TestLabel "parseInt_EmptyString" test14,
                                     TestLabel "parseInt_abc" test15,
                                     TestLabel "parseInt_123" test16,
                                     TestLabel "parseInt_123abc" test17,
                                     TestLabel "parseInt_abc123" test18,
                                     TestLabel "parseInt_-123" test19,
                                     TestLabel "parseInt_00000abc" test20,
                                     TestLabel "parseInt_-0" test21,
                                     TestLabel "parseInt_-123abc" test22,
                                     TestLabel "parseInt_--123" test23,
                                     TestLabel "parseInt_-abc" test24,
                                     TestLabel "isSpacing_space" test25,
                                     TestLabel "isSpacing_tab" test26,
                                     TestLabel "isSpacing_newline" test27,
                                     TestLabel "isSpacing_return" test28,
                                     TestLabel "isSpacing_leter" test29,
                                     TestLabel "isSpacing_number" test30,
                                     TestLabel "isSpacing_symbolic" test31,
                                     TestLabel "trimSpacing_NoSpaces" test32,
                                     TestLabel "trimSpacing_Mewline" test33,
                                     TestLabel "trimSpacing_LeadingSpace" test34,
                                     TestLabel "trimSpacing_TwoSpaces" test35,
                                     TestLabel "trimSpacing_LeadingTab" test36,
                                     TestLabel "trimSpacing_TwoTabs" test37,
                                     TestLabel "trimSpacing_MixedSpacing" test38,
                                     TestLabel "trimSpacing_IntermediateSpace" test39,
                                     TestLabel "parseNonEmpty_Nothing" test40,
                                     TestLabel "parseNonEmpty_Just" test41,
                                     TestLabel "parseId_Invalid1" test42,
                                     TestLabel "parseId_Invalid2" test43,
                                     TestLabel "parseId_LeadingDigit" test44,
                                     TestLabel "parseId_Valid" test45,
                                     TestLabel "parseSep_NoInput" test46,
                                     TestLabel "parseSep_EmptyStr" test47,
                                     TestLabel "parseSep_EmptySep" test48,
                                     TestLabel "parseSep_Match" test49,
                                     TestLabel "parseSep_LHSWhitespace" test50,
                                     TestLabel "parseSep_RHSWhitespace" test51,
                                     TestLabel "parseSep_Padded" test52,
                                     TestLabel "parseFromSeps_NoSeps" test53,
                                     TestLabel "parseFromSeps_MismatchedSeps" test54,
                                     TestLabel "parseFromSeps_OneSep" test55,
                                     TestLabel "parseFromSeps_ManySeps" test56,
                                     TestLabel "getErrPos_Empty" test57,
                                     TestLabel "getErrPos_Unparsed" test58,
                                     TestLabel "getErrPos_AllParsed" test59,
                                     TestLabel "getErrPos_SubstringOne" test60,
                                     TestLabel "getErrPos_SubstringTwo" test61,
                                     TestLabel "branchOnSpacing_EmptyString" test62,
                                     TestLabel "branchOnSpacing_Space" test63,
                                     TestLabel "branchOnSpacing_Tab" test64,
                                     TestLabel "branchOnSpacing_NonSpacing" test65,
                                     TestLabel "branchOnSpacing_MixedSpacing" test66,
                                     TestLabel "branchOnSpacing_MixedNonSpacing" test67,
                                     TestLabel "relToAbsErrPos_EmptyString" test68,
                                     TestLabel "relToAbsErrPos_Unparsed" test69,
                                     TestLabel "relToAbsErrPos_AllParsedOne" test70,
                                     TestLabel "relToAbsErrPos_AllParsedTwo" test71,
                                     TestLabel "relToAbsErrPos_SubstringNoPosOne" test72,
                                     TestLabel "relToAbsErrPos_SubstringNoPosTwo" test73,
                                     TestLabel "relToAbsErrPos_SubstringPosOne" test74,
                                     TestLabel "relToAbsErrPos_SubstringPosTwo" test75,
                                     TestLabel "stripComments_EmptyString" test76,
                                     TestLabel "stripComments_ManySymbols" test77,
                                     TestLabel "stripComments_Comments" test78,
                                     TestLabel "cleanLine_EmptyString" test79,
                                     TestLabel "cleanLine_NoTrimming" test80,
                                     TestLabel "cleanLine_LeadingSpacing" test81,
                                     TestLabel "cleanLine_Comments" test82,
                                     TestLabel "cleanLine_MixedPaddingOne" test83,
                                     TestLabel "cleanLine_MixedPaddingTwo" test84,
                                     TestLabel "iteOnSpacing_EmptyString" test85,
                                     TestLabel "iteOnSpacing_Space" test86,
                                     TestLabel "iteOnSpacing_Tab" test87,
                                     TestLabel "iteOnSpacing_NonSpacing" test88,
                                     TestLabel "iteOnSpacing_MixedSpacing" test89,
                                     TestLabel "iteOnSpacing_MixedNonSpacing" test90]

main = defaultMain tests
