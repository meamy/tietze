module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Maybe
import Lafont.Parse.DelimLists

-----------------------------------------------------------------------------------------
-- 

parseUnary :: Char -> String -> Maybe (Int, String)
parseUnary tok (c:line)
    | c == '|'  = Just (0, line)
    | c == tok  = maybeApply (\(n, str) -> (n + 1, str)) (parseUnary tok line)
    | otherwise = Nothing

parseUnaryA = parseUnary 'a'
parseUnaryB = parseUnary 'b'

test1 = TestCase (assertEqual "parseList can parse the empty list."
                              (Nothing :: Maybe ([Int], String))
                              (parseList parseUnaryA ',' ""))

test2 = TestCase (assertEqual "parseList can reject invalid list."
                              (Nothing :: Maybe ([Int], String))
                              (parseList parseUnaryA ',' "bsda , sdgs, dsadads , asd"))

test3 = TestCase (assertEqual "parseList can parse a singleton list."
                              (Just ([2], " fdaweas") :: Maybe ([Int], String))
                              (parseList parseUnaryA ',' "aa| fdaweas"))

test4 = TestCase (assertEqual "parseList can parse a three element list."
                              (Just ([1, 0, 3], " fdwea") :: Maybe ([Int], String))
                              (parseList parseUnaryA ',' "a|,|,aaa| fdwea"))

test5 = TestCase (assertEqual "parseList handles spacing."
                              (Just ([1, 0, 3], " fdwea") :: Maybe ([Int], String))
                              (parseList parseUnaryA ',' " \t  a| , \t |,   aaa| fdwea"))

test6 = TestCase (assertEqual "parseList rejects on a missing deliminator."
                              (Nothing :: Maybe ([Int], String))
                              (parseList parseUnaryA ',' "a|,|,aa|, fdwea"))

test7 = TestCase (assertEqual "parseList can parse using a different tokenizer."
                              (Just ([1, 0, 3], " fdwea") :: Maybe ([Int], String))
                              (parseList parseUnaryB ',' "b|,|,bbb| fdwea"))

test8 = TestCase (assertEqual "parseList can parse using a different deliminator."
                              (Just ([1, 0, 3], " fdwea") :: Maybe ([Int], String))
                              (parseList parseUnaryA ':' "a|:|:aaa| fdwea"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseList_EmptyString" test1,
                                     TestLabel "parseList_BadString" test2,
                                     TestLabel "parseList_Singleton" test3,
                                     TestLabel "parseList_Triple" test4,
                                     TestLabel "parseList_Spacing" test5,
                                     TestLabel "parseList_MissingDelim" test6,
                                     TestLabel "parseList_DifferentTokenizer" test7,
                                     TestLabel "parseList_DifferentDelimater" test2]

main = defaultMain tests
