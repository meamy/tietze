module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Named

-----------------------------------------------------------------------------------------
-- addToNamedList

nlist1 = addToNamedList "name1" []     [(1), (2), (3)] 1
nlist2 = addToNamedList "name2" nlist1 [(5), (6)]      10
nlist3 = addToNamedList "name3" nlist2 [(21)]          1

explist1 = [Named "name1" 1 (1), Named "name1" 2 (2), Named "name1" 3 (3)]
explist2 = [Named "name2" 10 (5), Named "name2" 11 (6)] ++ explist1
explist3 = [Named "name3" 1 (21)] ++ explist3

test1 = TestCase (assertEqual "addToNamedList (1/3)." explist1 nlist1)
test2 = TestCase (assertEqual "addToNamedList (2/3)." explist2 nlist2)
test3 = TestCase (assertEqual "addToNamedList (3/3)." explist3 nlist3)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "addToNamedList_1" test1,
                                     TestLabel "addToNamedList_2" test1,
                                     TestLabel "addToNamedList_3" test1]

main = defaultMain tests
