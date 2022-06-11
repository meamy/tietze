module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.Properties

-----------------------------------------------------------------------------------------
-- Test Data.

data Container = Container (Maybe Int) (Maybe Int) (Maybe Int) deriving (Eq,Show)

sampleSetter1 :: PropSetter Int Container
sampleSetter1 _ (Container (Just _) _ _)  = Nothing
sampleSetter1 v (Container Nothing x y)   = Just (Container (Just v) x y)

sampleSetter2 :: PropSetter Int Container
sampleSetter2 _ (Container _ (Just _) _)  = Nothing
sampleSetter2 v (Container x Nothing y)   = Just (Container x (Just v) y)

sampleSetter3 :: PropSetter Int Container
sampleSetter3 _ (Container _ _ (Just _))  = Nothing
sampleSetter3 v (Container x y Nothing)   = Just (Container x y (Just v))

settings :: [(String, PropUpdater Container)]
settings = [(makePropPair "val1" parseInt sampleSetter1),
            (makePropPair "val2" parseNat sampleSetter2),
            (makePropPair "val3" parseInt sampleSetter3)]

-----------------------------------------------------------------------------------------
-- makePropUpdater

updater1 :: PropUpdater Container
updater1 = snd (settings !! 0)

emptyCon :: Container
emptyCon = Container Nothing Nothing Nothing

containerWValOne :: Container
containerWValOne = Container (Just 100) Nothing Nothing

fullContainer :: Container
fullContainer = Container (Just 100) (Just 200) (Just 300)

test1 = TestCase (assertEqual "PropUpdater can parse an integer (1/2)."
                              (Right containerWValOne)
                              (updater1 "  100  " emptyCon))

test2 = TestCase (assertEqual "PropUpdater can parse an integer (2/2)."
                              (Right (Container (Just 50) Nothing Nothing))
                              (updater1 "  50  " emptyCon))

test3 = TestCase (assertEqual "PropUpdater can detect partial parse."
                              (Left (UnexpectedSymbol 5))
                              (updater1 "  100  xyz" emptyCon))

test4 = TestCase (assertEqual "PropUpdater can detect duplicate sets."
                              (Left (DuplicateProp "val1"))
                              (updater1 "  100  xyz" fullContainer))

test5 = TestCase (assertEqual "PropUpdater can detect bad parsing."
                              (Left (UnexpectedSymbol 2))
                              (updater1 "  abc" fullContainer))

-----------------------------------------------------------------------------------------
-- Property Dictionary.

dict0 :: PropertyDict Container
dict0 = empty

dict1 = addProp dict0 (settings !! 0)
dict2 = addProp dict1 (settings !! 1)
dict3 = addProp dict2 (settings !! 2)
dictn = addProps dict0 settings

test6 = TestCase (assertEqual "propToSeps on dict0."
                              ["@"]
                              (propsToSeps dict0))

test7 = TestCase (assertEqual "propToSeps on dict1."
                              ["@val1", "@"]
                              (propsToSeps dict1))

test8 = TestCase (assertEqual "propToSeps on dict2."
                              ["@val1", "@val2", "@"]
                              (propsToSeps dict2))

test9 = TestCase (assertEqual "propToSeps on dict3."
                              ["@val1", "@val2", "@val3", "@"]
                              (propsToSeps dict3))

test10 = TestCase (assertEqual "propToSeps on dictn."
                               ["@val1", "@val2", "@val3", "@"]
                               (propsToSeps dictn))

test11 = TestCase (assertEqual "Can parse from a dictionary."
                               (Right containerWValOne)
                               (parseFromPropDict dict3 "val1" "  100  " emptyCon))

test12 = TestCase (assertEqual "Can parse from a dictionary."
                               (Left (UnknownProp "val5"))
                               (parseFromPropDict dict3 "val5" "  100  " emptyCon))

-----------------------------------------------------------------------------------------
-- parsePropLine

seps :: [String]
seps = propsToSeps dict3

test13 = TestCase (assertEqual "parsePropLine can parse a property (1/2)."
                               (Right (Just containerWValOne :: Maybe Container))
                               (parsePropLine seps dict3 emptyCon "@val1 100  "))

test14 = TestCase (assertEqual "parsePropLine can parse a property (2/2)."
                               (Right res)
                               (parsePropLine seps dict3 emptyCon "@val2 100  "))
    where res = (Just (Container  Nothing (Just 100) Nothing) :: Maybe Container)

test15 = TestCase (assertEqual "parsePropLine can propogate an error."
                               (Left (UnexpectedSymbol 9))
                               (parsePropLine seps dict3 emptyCon "@val1 100 xyz"))

test16 = TestCase (assertEqual "parsePropLine can detect end of preamble."
                               (Right Nothing)
                               (parsePropLine seps dict3 emptyCon "rel1 x.y = z"))

test17 = TestCase (assertEqual "parsePropLine can detect bad properties."
                               (Left (UnknownProp "bad"))
                               (parsePropLine seps dict3 emptyCon "@bad value 5"))

test18 = TestCase (assertEqual "parsePropLine handles bad usage at the coding level."
                               (Left (ImplError "Property not prefixed with @."))
                               (parsePropLine ("val4":seps) dict3 emptyCon "val4 1"))

-----------------------------------------------------------------------------------------
-- parsePreamble

parseFn :: PropParser Container
parseFn = makePreambleParser dict3 emptyCon

ruleLine :: [String]
ruleLine = ["  rel1 x.y = z  -- comment"]

ruleLines :: [String]
ruleLines = ruleLine ++ ["rel2 y = z", "rel3 y = x"]

multiline :: [String]
multiline = ["", "@val1 100", "", "@val2 200", "", "", "", "@val3 300"] ++ ruleLines

-- Single line tests.

test19 = TestCase (assertEqual "PropParser can parse a single line."
                               (Right ([], 1, containerWValOne))
                               (parseFn ["  @val1  100  -- comment"] 0))

test20 = TestCase (assertEqual "PropParser can detect end of preamble."
                               (Right (ruleLine, 0, emptyCon))
                               (parseFn ruleLine 0))

test21 = TestCase (assertEqual "PropParser propogates errors."
                               (Left (0, UnexpectedSymbol 12))
                               (parseFn ["  @val1  100 xyz  -- comment"] 0))

test22 = TestCase (assertEqual "PropParser skips blank lines."
                               (Right ([], 1, emptyCon))
                               (parseFn ["   \t\t\t\t  -- comment"] 0))

-- Multi-line tests.

test23 = TestCase (assertEqual "PropParser handles valid preamble with body."
                               (Right (ruleLines, 8, fullContainer))
                               (parseFn multiline 0))

test24 = TestCase (assertEqual "PropParser handles invalid preamble with body."
                               (Left (2, DuplicateProp "val1"))
                               (parseFn ("@val1 100":multiline) 0))

-- Adjusted starting line.

test25 = TestCase (assertEqual "PropParser handles offsets (1/2)."
                               (Right (ruleLines, 18, fullContainer))
                               (parseFn multiline 10))

test26 = TestCase (assertEqual "PropParser handles offsets (2/2)."
                               (Left (12, DuplicateProp "val1"))
                               (parseFn ("@val1 100":multiline) 10))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "PropUpdater_Parse_TestOne" test1,
                                     TestLabel "PropUpdater_Parse_TestTwo" test2,
                                     TestLabel "PropUpdater_FailToParseAll" test3,
                                     TestLabel "PropUpdater_DuplicateProp" test4,
                                     TestLabel "PropUpdater_BadParsing" test5,
                                     TestLabel "propsToSteps_TestOne" test6,
                                     TestLabel "propsToSteps_TestTwo" test7,
                                     TestLabel "propsToSteps_TestThree" test8,
                                     TestLabel "propsToSteps_TestFour" test9,
                                     TestLabel "propsToSteps_TestFive" test10,
                                     TestLabel "parseFromPropDict_Valid" test11,
                                     TestLabel "parseFromPropDict_Valid" test12,
                                     TestLabel "parsePropLine_ValidOne" test13,
                                     TestLabel "parsePropLine_ValidTwo" test14,
                                     TestLabel "parsePropLine_PropogateError" test15,
                                     TestLabel "parsePropLine_EndOfPreamble" test16,
                                     TestLabel "parsePropLine_UnknownProp" test17,
                                     TestLabel "parsePropLine_InvalidUsage" test18,
                                     TestLabel "parsePreamble_Valid" test19,
                                     TestLabel "parsePreamble_EndOfPreamble" test20,
                                     TestLabel "parsePreamble_PropogateErrors" test21,
                                     TestLabel "parsePreamble_BlankLines" test22,
                                     TestLabel "parsePreamble_ValidMultiline" test23,
                                     TestLabel "parsePreamble_InvalidMultiline" test24,
                                     TestLabel "parsePreamble_OffsetValid" test25,
                                     TestLabel "parsePreamble_OffsetInvalid" test26]

main = defaultMain tests
