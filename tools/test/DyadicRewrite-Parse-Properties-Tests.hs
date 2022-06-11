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

emptyContainer :: Container
emptyContainer = Container Nothing Nothing Nothing

fullContainer :: Container
fullContainer = Container (Just 1) (Just 1) (Just 1)

test1 = TestCase (assertEqual "PropUpdater can parse an integer (1/2)."
                              (Right (Container (Just 100) Nothing Nothing))
                              (updater1 "  100  " emptyContainer))

test2 = TestCase (assertEqual "PropUpdater can parse an integer (2/2)."
                              (Right (Container (Just 50) Nothing Nothing))
                              (updater1 "  50  " emptyContainer))

test3 = TestCase (assertEqual "PropUpdater can detect partial parse."
                              (Left (UnexpectedSymbol 5))
                              (updater1 "  100  xyz" emptyContainer))

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
                               (Right (Container (Just 100) Nothing Nothing))
                               (parseFromPropDict dict3 "val1" "  100  " emptyContainer))

test12 = TestCase (assertEqual "Can parse from a dictionary."
                               (Left (UnknownProp "val5"))
                               (parseFromPropDict dict3 "val5" "  100  " emptyContainer))

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
                                     TestLabel "parseFromPropDict_Valid" test12]

main = defaultMain tests
