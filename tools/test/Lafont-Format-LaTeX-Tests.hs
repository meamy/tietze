module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Format.LaTeX
import Lafont.Generators.Semantics

-----------------------------------------------------------------------------------------
-- GenMacroList

gen1 :: (String, Maybe Int)
gen1 = ("abc_xyz", Just 1)

gen2 :: (String, Maybe Int)
gen2 = ("gen0", Just 2)

gen3 :: (String, Maybe Int)
gen3 = ("gen1", Nothing)

sampleDict0 :: GenDict Int
sampleDict0 = empty
sampleDict1 = addGen sampleDict0 gen1
sampleDict2 = addGen sampleDict1 gen2
sampleDict3 = addGen sampleDict2 gen3

test1 = TestCase (assertEqual "Can produce a empty GenMacroList from a GenDict."
                              ""
                              (toLaTeX $ makeGenMacros sampleDict0))

test2 = TestCase (assertEqual "Can produce a GenMacroList for a single generator."
                              (cmd1 ++ "\n")
                              (toLaTeX $ makeGenMacros sampleDict1))
    where cmd1 = "% Macro for: abc_xyz" ++ "\n" ++ "\\newcommand{\\lftgen0}{X_{0}}"

test3 = TestCase (assertEqual "Can produce a GenMacroList for a set of generators."
                              (cmd1 ++ "\n" ++ cmd2 ++ "\n" ++ cmd3 ++ "\n")
                              (toLaTeX $ makeGenMacros sampleDict3))
    where cmd1 = "% Macro for: abc_xyz" ++ "\n" ++ "\\newcommand{\\lftgen2}{X_{2}}"
          cmd2 = "% Macro for: gen0" ++ "\n" ++ "\\newcommand{\\lftgen1}{X_{1}}"
          cmd3 = "% Macro for: gen1" ++ "\n" ++ "\\newcommand{\\lftgen0}{X_{0}}"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "makeGenMacros_Empty" test1,
                                     TestLabel "makeGenMacros_1Gen" test2,
                                     TestLabel "makeGenMacros_3Gen" test3]

main = defaultMain tests
