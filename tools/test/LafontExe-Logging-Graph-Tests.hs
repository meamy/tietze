module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Sequence as Seq
import Lafont.Rewrite.Abstraction
import LafontExe.Logging.Graph

-----------------------------------------------------------------------------------------
-- printCycle

test1 = TestCase (assertEqual "printCycle handles length-1 cycles."
                              "\n1. Dep1"
                              (printCycle $ Seq.fromList ["Dep1"]))

test2 = TestCase (assertEqual "printCycle handles length-2 cycles."
                              "\n1. Dep1\n2. Dep2"
                              (printCycle $ Seq.fromList ["Dep1", "Dep2"]))

test3 = TestCase (assertEqual "printCycle handles length-3 cycles."
                              "\n1. Dep1\n2. Dep2\n3. Dep3"
                              (printCycle $ Seq.fromList ["Dep1", "Dep2", "Dep3"]))

-----------------------------------------------------------------------------------------
-- printUnmetDep

test4 = TestCase (assertEqual "printUnmetDep with unnamed source."
                              "Dep"
                              (printUnmetDep $ UnmetDep "" "Dep"))

test5 = TestCase (assertEqual "printUnmetDep with named source."
                              "Src -> Dep"
                              (printUnmetDep $ UnmetDep "Src" "Dep"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "printCycle_Len1" test1,
                                     TestLabel "printCycle_Len2" test2,
                                     TestLabel "printCycle_Len3" test3,
                                     TestLabel "printUnmetDep_Unnamed" test4,
                                     TestLabel "printUnmetDep_Named" test5]

main = defaultMain tests
