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
               ("Monoid" `isSubstrOf` (outputLines !! 0)),
               -- Generators are printed in reverse alphabetical order.
               ("xyz2" `isSubstrOf` (outputLines !! 1)),
               ("xyz1" `isSubstrOf` (outputLines !! 2)),
               ("lmnop123" `isSubstrOf` (outputLines !! 3)),
               ("abc2" `isSubstrOf` (outputLines !! 4)),
               ("abc1" `isSubstrOf` (outputLines !! 5))])
    else False
    where outputLines = lines str

test1 = (HandleTest "Good_Generators"
                    "Ensures that checkGenerators can print all monoid generators."
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
-- Valid Dyadic(2) file.

good_dyadic2_gen_file :: FilePath
good_dyadic2_gen_file = "data/test/dyadics/dyadic2.good.gens"

check3 :: String -> Bool
check3 str = (and [-- The model name.
                   ("Dyadic(2)" `isSubstrOf` str),
                   -- The x0 matrix.
                   ("x0 := [[0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0]]" `isSubstrOf` str),
                   -- The x1 matrix.
                   ("x1 := [[0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]" `isSubstrOf` str),
                   -- The gate1 matrix.
                   ("gate1 := [[1/2, 1/2, 1/2, 1/2], [1/2, -1/2, -1/2, 1/2], [1/2, -1/2, 1/2, -1/2], [1/2, 1/2, -1/2, -1/2]]" `isSubstrOf` str),
                   -- The gate2 matrix.
                   ("gate2 := [[1/2, 1/2, 1/2, 1/2], [1/2, -1/2, 1/2, -1/2], [1/2, -1/2, -1/2, 1/2], [1/2, 1/2, -1/2, -1/2]]" `isSubstrOf` str),
                   -- The gate3 matrix.
                   ("gate3 := [[0, 0, 1, 0], [0, 0, 0, 1], [0, 1, 0, 0], [1, 0, 0, 0]]" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 7)])

test3 = (HandleTest "Good_Dyadic(2)"
                    "Tests that Clifford(D)+Tof circuits can be parsed with 2 qubits."
                    (\x -> checkGenerators x good_dyadic2_gen_file)
                    check3)

-----------------------------------------------------------------------------------------
-- Invalid Dyadic(2) file.

bad_dyadic2_gen_file :: FilePath
bad_dyadic2_gen_file = "data/test/dyadics/dyadic2.bad.gens"

check4 :: String -> Bool
check4 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/dyadics/dyadic2.bad.gens:5" `isSubstrOf` str),
                   -- The expected error.
                   ("semv at 8" `isSubstrOf` str),
                   -- Error details.
                   ("gate position: X[5]" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test4 = (HandleTest "Bad_Dyadic(2)"
                    "Tests that Clifford(D)+Tof errors are detected for 2 qubits."
                    (\x -> checkGenerators x bad_dyadic2_gen_file)
                    check4)

-----------------------------------------------------------------------------------------
-- Valid Dyadic(3) file.

good_dyadic3_gen_file :: FilePath
good_dyadic3_gen_file = "data/test/dyadics/dyadic3.good.gens"

check5 :: String -> Bool
check5 str = (and [-- The model name.
                   ("Dyadic(3)" `isSubstrOf` str),
                   -- The x0 matrix.
                   ("x0 := [[0, 0, 0, 0, 1, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, 1, 0], [0, 0, 0, 0, 0, 0, 0, 1], [1, 0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0, 0]]" `isSubstrOf` str),
                   -- The x1 matrix.
                   ("x1 := [[0, 0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 1, 0], [0, 0, 0, 0, 0, 0, 0, 1], [0, 0, 0, 0, 1, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0, 0]]" `isSubstrOf` str),
                   -- The gate1 matrix.
                   ("gate1 := [[1/2, 0, 1/2, 0, 1/2, 0, 1/2, 0], [0, 1/2, 0, 1/2, 0, 1/2, 0, 1/2], [1/2, 0, -1/2, 0, 1/2, 0, -1/2, 0], [0, 1/2, 0, -1/2, 0, -1/2, 0, 1/2], [1/2, 0, 1/2, 0, -1/2, 0, -1/2, 0], [0, 1/2, 0, 1/2, 0, -1/2, 0, -1/2], [0, 1/2, 0, -1/2, 0, 1/2, 0, -1/2], [1/2, 0, -1/2, 0, -1/2, 0, 1/2, 0]]" `isSubstrOf` str),
                   -- The gate2 matrix.
                   ("gate2 := [[1/2, 1/2, 1/2, 1/2, 0, 0, 0, 0], [1/2, -1/2, 1/2, -1/2, 0, 0, 0, 0], [1/2, 1/2, -1/2, -1/2, 0, 0, 0, 0], [1/2, -1/2, -1/2, 1/2, 0, 0, 0, 0], [0, 0, 0, 0, 1/2, 1/2, 1/2, 1/2], [0, 0, 0, 0, 1/2, -1/2, 1/2, -1/2], [0, 0, 0, 0, 1/2, -1/2, -1/2, 1/2], [0, 0, 0, 0, 1/2, 1/2, -1/2, -1/2]]" `isSubstrOf` str),
                   -- The gate3 matrix.
                   ("gate3 := [[0, 0, 0, 0, 1, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, 1, 0], [0, 0, 0, 0, 0, 0, 0, 1], [1, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0, 0]]" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 7)])

test5 = (HandleTest "Good_Dyadic(3)"
                    "Tests that Clifford(D)+Tof circuits can be parsed with 3 qubits."
                    (\x -> checkGenerators x good_dyadic3_gen_file)
                    check5)

-----------------------------------------------------------------------------------------
-- Invalid Dyadic(3) file.

bad_dyadic3_gen_file :: FilePath
bad_dyadic3_gen_file = "data/test/dyadics/dyadic3.bad.gens"

check6 :: String -> Bool
check6 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/dyadics/dyadic3.bad.gens:5" `isSubstrOf` str),
                   -- The expected error.
                   ("semv at 8" `isSubstrOf` str),
                   -- Error details.
                   ("gate position: X[5]" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test6 = (HandleTest "Bad_Dyadic(3)"
                    "Tests that Clifford(D)+Tof errors are detected for 3 qubits."
                    (\x -> checkGenerators x bad_dyadic3_gen_file)
                    check6)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = [test1, test2, test3, test4, test5, test6]

main = handleTestToMain $ runAllHandleTests tests
