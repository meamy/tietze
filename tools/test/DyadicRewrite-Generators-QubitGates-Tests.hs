module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Generators.QubitGates

import Quantum.Synthesis.Matrix
import Quantum.Synthesis.Ring

-----------------------------------------------------------------------------------------
-- Helpers.

-- | Applies a dyadic phase to a dyadic state.
dyadPhase :: (Nat n) => Dyadic -> Matrix n One Dyadic -> Matrix n One Dyadic
dyadPhase phase m = phase `scalarmult` m

-- | Applies a phase from D[sqrt(2)] to a dyadic state. When passed a phase of 1, this
-- function also embeds dyadic states into the space of states over D[sqrt(2)]. This is
-- sometimes necessary for tests to typecheck.
drPhase :: (Nat n) => DRootTwo -> Matrix n One Dyadic -> Matrix n One DRootTwo
drPhase phase m = phase `scalarmult` (matrix_map fromDyadic m)

-- | Applies a phase from D[i,sqrt(2)] to a dyadic state. When passed a phase of 1, this
-- function also embeds dyadic states into the space of states over D[i,sqrt(2)]. This is
-- sometimes necessary for tests to typecheck.
drcPhase :: (Nat n) => DRComplex -> Matrix n One Dyadic -> Matrix n One DRComplex
drcPhase phase m = phase `scalarmult` (matrix_map fromDyadic m)

-----------------------------------------------------------------------------------------
-- Tests the implementation of each one-qubit gate.

state_0 :: Matrix Two One Dyadic
state_0 = matrix_of_columns [[1, 0]]

state_1 :: Matrix Two One Dyadic
state_1 = matrix_of_columns [[0, 1]]

state_plus :: Matrix Two One DRootTwo
state_plus = matrix_of_columns [[roothalf, roothalf]]

state_minus :: Matrix Two One DRootTwo
state_minus = matrix_of_columns [[roothalf, -roothalf]]

state_1c :: Matrix Two One DRComplex
state_1c = matrix_map fromDyadic state_1

test1 = TestCase (assertEqual "Checks gate_id sends |0> to |0>."
                              state_0 (gate_id .*. state_0))

test2 = TestCase (assertEqual "Checks gate_id sends |1> to |1>."
                              state_1 (gate_id .*. state_1))

test3 = TestCase (assertEqual "Checks gate_x sends |0> to |1>."
                              state_1 (gate_x .*. state_0))

test4 = TestCase (assertEqual "Checks gate_x sends |1> to |0>."
                              state_0 (gate_x .*. state_1))

test5 = TestCase (assertEqual "Checks gate_y sends |0> to i|1>."
                              (drcPhase i state_1) (gate_y .*. (drcPhase 1 state_0)))

test6 = TestCase (assertEqual "Checks gate_y sends |1> to -i|0>."
                              (drcPhase (-i) state_0) (gate_y .*. (drcPhase 1 state_1)))

test7 = TestCase (assertEqual "Checks gate_x sends |0> to |0>."
                              state_0 (gate_z .*. state_0))

test8 = TestCase (assertEqual "Checks gate_x sends |1> to -|1>."
                              (dyadPhase (-1) state_1) (gate_z .*. state_1))

test9 = TestCase (assertEqual "Checks gate_h sends |0> to |+>."
                              state_plus (gate_h .*. (drPhase 1 state_0)))

test10 = TestCase (assertEqual "Checks gate_h sends |0> to |->."
                              state_minus (gate_h .*. (drPhase 1 state_1)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "IDTest1" test1,
                                     TestLabel "IDTest2" test2,
                                     TestLabel "XTest1" test3,
                                     TestLabel "XTest2" test4,
                                     TestLabel "YTest1" test5,
                                     TestLabel "YTest2" test6,
                                     TestLabel "ZTest1" test7,
                                     TestLabel "ZTest2" test8,
                                     TestLabel "HTest1" test9,
                                     TestLabel "HTest2" test10]

main = defaultMain tests
