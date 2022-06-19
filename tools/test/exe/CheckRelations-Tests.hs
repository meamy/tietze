module Main where

import Data.List
import LafontExeTest.HandleTest
import LafontExe.CheckRelations
import Lafont.String

-----------------------------------------------------------------------------------------
-- Common files.

gen_file :: FilePath
gen_file = "data/test/example.gens" 

-----------------------------------------------------------------------------------------
-- Good input.

good_rel_file :: FilePath
good_rel_file = "data/test/example.rels"

check1 :: String -> Bool
check1 str =
    if ((length outputLines) == 3)
    then (and [("rel1: abc1.xyz1.abc2.xyz2 = lmnop123" == (outputLines !! 0)),
               ("rel2: abc1.abc1.abc1.abc1 = lmnop123" == (outputLines !! 1)),
               ("rel3: lmnop123.lmnop123 → ε" == (outputLines !! 2))])
    else False
    where outputLines = lines str

test1 = (HandleTest "Good_Relations"
                    "Ensures that checkRelations can print all relations."
                    (\x -> checkRelations x gen_file good_rel_file)
                    check1)

-----------------------------------------------------------------------------------------
-- Bad input.

bad_rel_file :: FilePath
bad_rel_file = "data/test/bad.rels"

check2 :: String -> Bool
check2 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.rels:4" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("xyz3" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test2 = (HandleTest "Bad_Relation"
                    "Tests that a relation file with invalid generator is rejected."
                    (\x -> checkRelations x gen_file bad_rel_file)
                    check2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = [test1, test2]

main = handleTestToMain $ runAllHandleTests tests
