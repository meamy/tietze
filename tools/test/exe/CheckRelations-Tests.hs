module Main where

import Data.List
import LafontExeTest.HandleTest
import LafontExe.CheckRelations
import Lafont.String

-----------------------------------------------------------------------------------------
-- Common files.

goodGens :: FilePath
goodGens = "data/test/example.gens" 

goodRels :: FilePath
goodRels = "data/test/example.rels"

-----------------------------------------------------------------------------------------
-- Good input.

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
                    (\x -> checkRelations x goodGens goodRels)
                    check1)

-----------------------------------------------------------------------------------------
-- Bad generators.

badGens :: FilePath
badGens = "data/test/bad.gens" 

check2 :: String -> Bool
check2 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/bad.gens:5" `isSubstrOf` str),
                   -- The expected error.
                   ("invalid symbol" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test2 = (HandleTest "Bad_generators"
                    "Tests that a generator file with invalid symbol is rejected."
                    (\x -> checkRelations x badGens goodRels)
                    check2)

-----------------------------------------------------------------------------------------
-- Bad relations.

badRels :: FilePath
badRels = "data/test/bad.rels"

check3 :: String -> Bool
check3 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.rels:4" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("xyz3" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test3 = (HandleTest "Bad_Relation"
                    "Tests that a relation file with invalid generator is rejected."
                    (\x -> checkRelations x goodGens badRels)
                    check3)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = [test1, test2, test3]

main = handleTestToMain $ runAllHandleTests tests
