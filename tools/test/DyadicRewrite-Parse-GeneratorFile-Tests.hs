module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Either
import DyadicRewrite.Generators.Semantics
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.GeneratorFile

-----------------------------------------------------------------------------------------
-- parseGenerator

doFailedParse :: String -> String -> Either String String
doFailedParse msg _ = Left msg

readParse :: String -> Either String Int
readParse str = (Right (read str :: Int))

test0 = TestCase (assertEqual "Tests that empty strings are rejected."
                              (Left (Right InvalidGenName))
                              (parseGenerator (doFailedParse "") ""))

test1 = TestCase (assertEqual "Tests that bad generator ID's are rejected."
                              (Left (Right InvalidGenName))
                              (parseGenerator (doFailedParse "") "1abc"))

test2 = TestCase (assertEqual "Tests that a lone generator can be read."
                              (Right ("abc", Nothing))
                              (parseGenerator (doFailedParse "") "  abc  "))

test3 = TestCase (assertEqual "Tests that a lone generator can be read."
                              (Left (Left (UnexpectedSymbol 5)))
                              (parseGenerator (doFailedParse "") "  abc  abc"))

test4 = TestCase (assertEqual "Tests that a bad semantic value is rejected (1/2)."
                              (Left (Right (InvalidGenSem 9 "abc")))
                              (parseGenerator (doFailedParse "abc") "  abc  :=  512"))

test5 = TestCase (assertEqual "Tests that a bad semantic value is rejected (2/2)."
                              (Left (Right (InvalidGenSem 8 "123 45")))
                              (parseGenerator (doFailedParse "123 45") "  abc :=  512"))

test6 = TestCase (assertEqual "Tests that a good semantic value is accepted (1/2)."
                              (Right ("abc", Just 512))
                              (parseGenerator readParse "  abc :=  512"))

test7 = TestCase (assertEqual "Tests that a good semantic value is accepted (2/2)."
                              (Right ("abc", Just 4321))
                              (parseGenerator readParse "  abc :=  4321 "))

-----------------------------------------------------------------------------------------
-- updateGenerators

copyStr :: String -> Either String String
copyStr semStr = Right semStr

gen1 :: (String, Maybe String)
gen1 = ("xyx", Just "qwerty")

gen2 :: (String, Maybe String)
gen2 = ("x123", Just "lmnop")

emptyDict :: GenDict String
emptyDict = empty

dupDict = addGen emptyDict ("abc", Nothing)
dict1 = addGen emptyDict gen1
dict2 = addGen dict1 gen2

test8 = TestCase (assertEqual "Tests that updateGenerators propogates errors (1/2)."
                              (Left (Right InvalidGenName))
                              (updateGenerators (doFailedParse "") emptyDict "1abc"))

test9 = TestCase (assertEqual "Tests that updateGenerators propogates errors (2/2)."
                              (Left (Right (InvalidGenSem 5 "1")))
                              (updateGenerators (doFailedParse "1") emptyDict "abc:=1"))

test10 = TestCase (assertEqual "Tests that updateGenerators detects duplicate names."
                               (Left (Right (DuplicateGenName "abc")))
                               (updateGenerators copyStr dupDict "abc:=1"))

test11 = TestCase (assertEqual "Tests that updateGenerators adds new generators."
                               (Just "1" :: Maybe String)
                               semv)
         where semv = case (updateGenerators copyStr emptyDict "abc:=1") of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

test12 = TestCase (assertEqual "Tests that updateGenerators preserves generators (1/3)."
                               (snd gen1)
                               semv)
         where semv = case (updateGenerators copyStr dict2 "abc:=1") of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict (fst gen1)

test13 = TestCase (assertEqual "Tests that updateGenerators preserves generators (2/3)."
                               (snd gen2)
                               semv)
         where semv = case (updateGenerators copyStr dict2 "abc:=1") of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict (fst gen2)

test14 = TestCase (assertEqual "Tests that updateGenerators preserves generators (3/3)."
                               (Just "1" :: Maybe String)
                               semv)
         where semv = case (updateGenerators copyStr dict2 "abc:=1") of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

test15 = TestCase (assertEqual "Tests that updateGenerators supports no semantics."
                               Nothing
                               semv)
         where semv = case (updateGenerators copyStr emptyDict "abc") of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

-----------------------------------------------------------------------------------------
-- parseSemanticModel

-- Single line tests.

test16 = TestCase (assertEqual "Tests parsing Monoidal semantics from a single line."
                               (Right (MonoidalSem, 0, []))
                               (parseSemanticModel ["    Monoidal    -- is valid"] 0))

test17 = TestCase (assertEqual "Tests parsing Dyadic(1) semantics from a single line."
                               (Right (DyadicOneSem, 0, []))
                               (parseSemanticModel ["    Dyadic(1)    --  is valid"] 0))

test18 = TestCase (assertEqual "Tests parsing Dyadic(1) semantics from a single line."
                               (Right (DyadicTwoSem, 0, []))
                               (parseSemanticModel ["    Dyadic(2)    -- is valid"] 0))

test19 = TestCase (assertEqual "Tests parsing Dyadic(1) semantics from a single line."
                               (Right (DyadicThreeSem, 0, []))
                               (parseSemanticModel ["    Dyadic(3)    -- is valid"] 0))

test20 = TestCase (assertEqual "Tests parsing an unknown semantic model."
                               (Left (0, (Right (UnknownSemModel "Unknown"))))
                               (parseSemanticModel ["    Unknown"] 0))

test21 = TestCase (assertEqual "Tests parsing a valid model followed by other sybmols."
                               (Left (0, (Right (UnknownSemModel "Monoidal Unknown"))))
                               (parseSemanticModel ["    Monoidal Unknown"] 0))

-- Multi-line tests.

test22 = TestCase (assertEqual "Tests parsing a semantic model from an empty line."
                               (Left (0, (Right MissingSemModel)))
                               (parseSemanticModel [] 0))

test23 = TestCase (assertEqual "Tests parsing a semantic model after many empty lines."
                               (Right (MonoidalSem, 5, []))
                               (parseSemanticModel input 0))
    where input = ["", "   -- comment", " \t \t   -- comment", "", "  ", "   Monoidal "]

test24 = TestCase (assertEqual "Tests parsing a semantic model with lines after model."
                               (Right (MonoidalSem, 4, post))
                               (parseSemanticModel input 0))
    where post = ["a", "b", "c", "d"]
          input = ["", "", "", "", "Monoidal"] ++ post

test25 = TestCase (assertEqual "Tests returning an error after multiple lines."
                               (Left (4, (Right (UnknownSemModel "Unknown"))))
                               (parseSemanticModel ["", "", "", "", "Unknown"] 0))

-- Adjusted starting line.

test26 = TestCase (assertEqual "Tests parsing a semantic model with line offset."
                               (Right (MonoidalSem, 9, post))
                               (parseSemanticModel input 5))
    where post = ["a", "b", "c", "d"]
          input = ["", "", "", "", "Monoidal"] ++ post

test27 = TestCase (assertEqual "Tests parsing an error with a line offset."
                               (Left (9, (Right (UnknownSemModel "Unknown"))))
                               (parseSemanticModel ["", "", "", "", "Unknown"] 5))

-----------------------------------------------------------------------------------------
-- parseGenFile

-- Single line tests.

test28 = TestCase (assertEqual "Tests parsing of a single generator with parseGenFile."
                               (Just "   1   " :: Maybe String)
                               semv)
         where semv = case (parseGenFile copyStr ["  abc :=   1   -- comment"] 0) of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

test29 = TestCase (assertEqual "Tests that parseGenFile handles single line errors."
                               (Left (0, (Right InvalidGenName)))
                               (parseGenFile copyStr ["  1abc :=   1   -- comment"] 0))

-- Multi-line tests.

validMultiline :: [String]
validMultiline = ["", "abc:= 12a", "cdf:=32c ", "", "xyz_123:=qwerty"]

test30 = TestCase (assertEqual "Tests parsing of a single generator with multiple lines."
                               (Just "1" :: Maybe String)
                               semv)
         where input =  [" \t\t\t  \t", "  \t\t", "abc:=1", "  -- comment"]
               semv = case (parseGenFile copyStr input 0) of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

test31 = TestCase (assertEqual "Tests parsing of multiple valid generators (1/3)."
                               (Just " 12a" :: Maybe String)
                               semv)
         where semv = case (parseGenFile copyStr validMultiline 0) of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

test32 = TestCase (assertEqual "Tests parsing of multiple valid generators (2/3)."
                               (Just "32c " :: Maybe String)
                               semv)
         where semv = case (parseGenFile copyStr validMultiline 0) of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "cdf"

test33 = TestCase (assertEqual "Tests parsing of multiple valid generators (3/3)."
                               (Just "qwerty" :: Maybe String)
                               semv)
         where semv = case (parseGenFile copyStr validMultiline 0) of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "xyz_123"

test34 = TestCase (assertEqual "Tests that parseGenFile handles errors after valid lines."
                               (Left (1, (Right (DuplicateGenName "b"))))
                               (parseGenFile copyStr ["a", "b", "c", "b", "d"] 0))

-- Adjusted starting line.

test35 = TestCase (assertEqual "Tests that parseGenFile handles offsets (1/2)."
                               (Just "1" :: Maybe String)
                               semv)
         where semv = case (parseGenFile copyStr ["", "", "", "abc:=1", "", ""] 5) of
                          Left _     -> Nothing
                          Right dict -> interpretGen dict "abc"

test36 = TestCase (assertEqual "Tests that parseGenFile handles offsets (2/2)."
                               (Left (8, (Right InvalidGenName)))
                               (parseGenFile copyStr ["", "", "", "1abc:=1", "", ""] 5))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseGenerator_EmptyString" test0,
                                     TestLabel "parseGenerator_BadId" test1,
                                     TestLabel "parseGenerator_OnlyId" test2,
                                     TestLabel "parseGenerator_IdNoSemEq" test3,
                                     TestLabel "parseGenerator_BadSemOne" test4,
                                     TestLabel "parseGenerator_BadSemTwo" test5,
                                     TestLabel "parseGenerator_GoodSemOne" test6,
                                     TestLabel "parseGenerator_GoodSemTwo" test7,
                                     TestLabel "updateGenerators_PropogateOne" test8,
                                     TestLabel "updateGenerators_PropogateTwo" test9,
                                     TestLabel "updateGenerators_DetectsDupName" test10,
                                     TestLabel "updateGenerators_AddNewGen" test11,
                                     TestLabel "updateGenerators_UpdateGens_0" test12,
                                     TestLabel "updateGenerators_UpdateGens_1" test13,
                                     TestLabel "updateGenerators_UpdateGens_2" test14,
                                     TestLabel "updateGenerators_NoSemantics" test15,
                                     TestLabel "parseSemanticModel_Monoidal" test16,
                                     TestLabel "parseSemanticModel_Dyadic(1)" test17,
                                     TestLabel "parseSemanticModel_Dyadic(2)" test18,
                                     TestLabel "parseSemanticModel_Dyadic(3)" test19,
                                     TestLabel "parseSemanticModel_UnknownModel" test20,
                                     TestLabel "parseSemanticModel_PartialModel" test21,
                                     TestLabel "parseSemanticModel_EmptyInput" test22,
                                     TestLabel "parseSemanticModel_EmptyLines" test23,
                                     TestLabel "parseSemanticModel_ReturnsLines" test24,
                                     TestLabel "parseSemanticModel_NthLineError" test25,
                                     TestLabel "parseSemanticModel_RvOffset" test26,
                                     TestLabel "parseSemanticModel_ErrorOffset" test27,
                                     TestLabel "parseGenFile_ValidGen" test28,
                                     TestLabel "parseGenFile_InvalidGen" test29,
                                     TestLabel "parseGenFile_ValidGenMultiline" test30,
                                     TestLabel "parseGenFile_MultipleGensOne" test31,
                                     TestLabel "parseGenFile_MultipleGensTwo" test32,
                                     TestLabel "parseGenFile_MultipleGensThree" test33,
                                     TestLabel "parseGenFile_MidParsingError" test34,
                                     TestLabel "parseGenFile_OffsetValid" test35,
                                     TestLabel "parseGenFile_OffsetInvalid" test36]

main = defaultMain tests
