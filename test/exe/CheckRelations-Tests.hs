module Main where

import qualified Data.List.NonEmpty       as NonEmpty
import           Data.List
import           TietzeExeTest.HandleTest
import           TietzeExe.CheckRelations
import           Tietze.String
import           System.IO

-----------------------------------------------------------------------------------------
-- Common files.

goodGens :: FilePath
goodGens = "data/test/example.gens" 

goodRels :: FilePath
goodRels = "data/test/example.rels"

dyadic2Gens :: FilePath
dyadic2Gens = "data/test/dyadics/dyadic2.good.gens"

dyadic3Gens :: FilePath
dyadic3Gens = "data/test/dyadics/dyadic3.good.gens"

-----------------------------------------------------------------------------------------
-- Utilities.

doTest :: String -> String -> Handle -> IO ()
doTest genFname relFname x = checkRelations x genFname relFname'
    where relFname' = NonEmpty.fromList [relFname]

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
                    (doTest goodGens goodRels)
                    check1)

-----------------------------------------------------------------------------------------
-- Bad generators.

badGens :: FilePath
badGens = "data/test/bad.gens" 

check2 :: String -> Bool
check2 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/bad.gens:6" `isSubstrOf` str),
                   -- The expected error.
                   ("invalid symbol" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test2 = (HandleTest "Bad_Generators"
                    "Tests that a generator file with invalid symbol is rejected."
                    (doTest badGens goodRels)
                    check2)

-----------------------------------------------------------------------------------------
-- Bad relations.

badRels :: FilePath
badRels = "data/test/bad.rels"

check3 :: String -> Bool
check3 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.rels:5" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("xyz3" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test3 = (HandleTest "Bad_Relation"
                    "Tests that a relation file with invalid generator is rejected."
                    (doTest goodGens badRels)
                    check3)

-----------------------------------------------------------------------------------------
-- Validation functions for semantic tests.

checkGoodSem :: String -> Bool
checkGoodSem str = (and [-- Finding this symbol suggests a relation list was returned.
                         ("→" `isSubstrOf` str),
                         -- There should be 4 relations.
                         ((length (lines str)) == 4)])

checkBadSem :: String -> Bool
checkBadSem str = (and [-- Correct error message was displayed.
                       ("contradicts semantics" `isSubstrOf` str),
                       -- Correct relation was displayed.
                       ("rx" `isSubstrOf` str),
                       -- Should be a single line.
                       ((length (lines str)) == 1)])

checkMissingSem :: String -> Bool
checkMissingSem str = (and [-- Correct error message was displayed.
                            ("contains an unknown generator:" `isSubstrOf` str),
                            -- Correct relation was displayed.
                            ("rx" `isSubstrOf` str),
                            -- Should be a single line.
                            ((length (lines str)) == 1)])

-----------------------------------------------------------------------------------------
-- Good semantics using dyadics.

goodDyadicRels :: FilePath
goodDyadicRels = "data/test/dyadics/dyadic.good.rels"

test4 = (HandleTest "Good_RelSem_Dyadic2"
                    "Tests that sound Dyadic(2) relations pass validation."
                    (doTest dyadic2Gens goodDyadicRels)
                    checkGoodSem)

test5 = (HandleTest "Good_RelSem_Dyadic3"
                    "Tests that sound Dyadic(3) relations pass validation."
                    (doTest dyadic3Gens goodDyadicRels)
                    checkGoodSem)

-----------------------------------------------------------------------------------------
-- Bad semantics using dyadics.

badDyadicRels :: FilePath
badDyadicRels = "data/test/dyadics/dyadic.bad.rels"

test6 = (HandleTest "Bad_RelSem_Dyadic2"
                    "Tests that unsound Dyadic(2) relations fail validation."
                    (doTest dyadic2Gens badDyadicRels)
                    checkBadSem)

test7 = (HandleTest "Bad_RelSem_Dyadic3"
                    "Tests that unsound Dyadic(3) relations fail validation."
                    (doTest dyadic3Gens badDyadicRels)
                    checkBadSem)

-----------------------------------------------------------------------------------------
-- Missing relations using dyadics.

missingDyadicRels :: FilePath
missingDyadicRels = "data/test/dyadics/dyadic.missing.rels"

test8 = (HandleTest "Missing_RelSem_Dyadic2"
                    "Tests that missing generators abort Dyadic(2) validation."
                    (doTest dyadic2Gens missingDyadicRels)
                    checkMissingSem)

test9 = (HandleTest "Missing_RelSem_Dyadic3"
                    "Tests that missing generators abort Dyadic(3) validation."
                    (doTest dyadic3Gens missingDyadicRels)
                    checkMissingSem)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9]

main = handleTestToMain $ runAllHandleTests tests
