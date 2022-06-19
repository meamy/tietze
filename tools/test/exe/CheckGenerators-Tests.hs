module Main where

import Data.List
import LafontExeTest.HandleTest
import LafontExe.CheckGenerators
import Lafont.String

-----------------------------------------------------------------------------------------
-- Good input.

good_gen_file :: FilePath
good_gen_file = "data/test/example.gens"

check1 :: String -> Bool
check1 str =
    if ((length outputLines) == 6)
    then (and [-- Semantic model check.
               ("Monoidal" `isSubstrOf` (outputLines !! 0)),
               -- Generators are printed in reverse alphabetical order.
               ("xyz2" `isSubstrOf` (outputLines !! 1)),
               ("xyz1" `isSubstrOf` (outputLines !! 2)),
               ("lmnop123" `isSubstrOf` (outputLines !! 3)),
               ("abc2" `isSubstrOf` (outputLines !! 4)),
               ("abc1" `isSubstrOf` (outputLines !! 5))])
    else False
    where outputLines = lines str

test1 = (HandleTest "Good_Generators"
                    "Ensures that checkGenerators can print all monoidal generators."
                    (\x -> checkGenerators x good_gen_file)
                    check1)

-----------------------------------------------------------------------------------------
-- Bad input.

bad_gen_file :: FilePath
bad_gen_file = "data/test/bad.gens"

check2 :: String -> Bool
check2 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/bad.gens:5" `isSubstrOf` str),
                   -- The expected error.
                   ("invalid symbol" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test2 = (HandleTest "Bad_Generators"
                    "Tests that a generator file with invalid characters is rejected."
                    (\x -> checkGenerators x bad_gen_file)
                    check2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = [test1, test2]

main = handleTestToMain $ runAllHandleTests tests
