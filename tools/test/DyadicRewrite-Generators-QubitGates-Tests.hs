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
-- Tests the implementation of the two-qubit templates.

state_00 = state_0 `tensor` state_0
state_01 = state_0 `tensor` state_1
state_10 = state_1 `tensor` state_0
state_11 = state_1 `tensor` state_1

test11 = TestCase (assertEqual "Checks X[0] is gate_x `tensor` gate_id."
                               (gate_x `tensor` gate_id) gate_x0)
         where gate_x0 = prepare_gate_4x4 (OneQubitOp4x4 gate_x TopBit)

test12 = TestCase (assertEqual "Checks X[1] is gate_id `tensor` gate_x."
                               (gate_id `tensor` gate_x) gate_x1)
         where gate_x1 = prepare_gate_4x4 (OneQubitOp4x4 gate_x BotBit)

test13 = TestCase (assertEqual "Checks SWAP[0][1] = SWAP."
                              gate_swap gate_swap01)
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test14 = TestCase (assertEqual "Checks SWAP[1][0] = SWAP."
                              gate_swap gate_swap10)
         where gate_swap10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap BotBit)

test15 = TestCase (assertEqual "Checks SWAP[0][1] sends |00> to |00>."
                              state_00 (gate_swap01 .*. state_00))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test16 = TestCase (assertEqual "Checks SWAP[0][1] sends |01> to |10>."
                              state_10 (gate_swap01 .*. state_01))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test17 = TestCase (assertEqual "Checks SWAP[0][1] sends |10> to |01>."
                              state_01 (gate_swap01 .*. state_10))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test18 = TestCase (assertEqual "Checks SWAP[0][1] sends |11> to |11>."
                              state_11 (gate_swap01 .*. state_11))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test19 = TestCase (assertEqual "Checks CX[0][1] sends |00> to |00>."
                               state_00 (gate_cx01 .*. state_00))
         where gate_cx01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx TopBit)

test20 = TestCase (assertEqual "Checks CX[0][1] sends |01> to |01>."
                              state_01 (gate_cx01 .*. state_01))
         where gate_cx01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx TopBit)

test21 = TestCase (assertEqual "Checks CX[0][1] sends |10> to |11>."
                              state_11 (gate_cx01 .*. state_10))
         where gate_cx01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx TopBit)

test22 = TestCase (assertEqual "Checks CX[0][1] sends |11> to |10>."
                              state_10 (gate_cx01 .*. state_11))
         where gate_cx01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx TopBit)

test23 = TestCase (assertEqual "Checks CX[1][0] sends |00> to |00>."
                               state_00 (gate_cx10 .*. state_00))
         where gate_cx10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx BotBit)

test24 = TestCase (assertEqual "Checks CX[1][0] sends |01> to |11>."
                              state_11 (gate_cx10 .*. state_01))
         where gate_cx10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx BotBit)

test25 = TestCase (assertEqual "Checks CX[1][0] sends |10> to |10>."
                              state_10 (gate_cx10 .*. state_10))
         where gate_cx10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx BotBit)

test26 = TestCase (assertEqual "Checks CX[0][1] sends |11> to |01>."
                              state_01 (gate_cx10 .*. state_11))
         where gate_cx10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cx BotBit)

test27 = TestCase (assertEqual "Checks CZ[0][1] is CZ[1][0]."
                              gate_cz01 gate_cz10)
         where gate_cz01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cz TopBit)
               gate_cz10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cz BotBit)

test28 = TestCase (assertEqual "Checks CZ[0][1] sends |00> to |00>."
                               state_00 (gate_cz01 .*. state_00))
         where gate_cz01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cz TopBit)

test29 = TestCase (assertEqual "Checks CZ[0][1] sends |01> to |01>."
                               state_01 (gate_cz01 .*. state_01))
         where gate_cz01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cz TopBit)

test30 = TestCase (assertEqual "Checks CZ[0][1] sends |10> to |10>."
                               state_10 (gate_cz01 .*. state_10))
         where gate_cz01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cz TopBit)

test31 = TestCase (assertEqual "Checks CZ[0][1] sends |11> to -|11>."
                               ((-1) `scalarmult` state_11) (gate_cz01 .*. state_11))
         where gate_cz01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_cz TopBit)

test32 = TestCase (assertEqual "Checks K[0][1] is H tensor H."
                               (gate_h `tensor` gate_h) gate_k01)
         where typed_gate = gate_k :: Matrix Four Four DRootTwo
               gate_k01 = prepare_gate_4x4 (TwoQubitOp4x4 typed_gate TopBit)

test33 = TestCase (assertEqual "Checks K[1][0] is H tensor H."
                               (gate_h `tensor` gate_h) gate_k10)
         where typed_gate = gate_k :: Matrix Four Four DRootTwo
               gate_k10 = prepare_gate_4x4 (TwoQubitOp4x4 typed_gate BotBit)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "2x2_ID_Test1" test1,
                                     TestLabel "2x2_ID_Test2" test2,
                                     TestLabel "2x2_X_Test1" test3,
                                     TestLabel "2x2_X_Test2" test4,
                                     TestLabel "2x2_Y_Test1" test5,
                                     TestLabel "2x2_Y_Test2" test6,
                                     TestLabel "2x2_Z_Test1" test7,
                                     TestLabel "2x2_Z_Test2" test8,
                                     TestLabel "2x2_H_Test1" test9,
                                     TestLabel "2x2_H_Test2" test10,
                                     TestLabel "4x4_X[0]_MatrixTest" test11,
                                     TestLabel "4x4_X[1]_MatrixTest" test12,
                                     TestLabel "4x4_SWAP[0][1]_MatrixTest" test13,
                                     TestLabel "4x4_SWAP[0][1]_Is_SWAP[1][0]" test14,
                                     TestLabel "4x4_SWAP[0][1]_Test1" test15,
                                     TestLabel "4x4_SWAP[0][1]_Test2" test16,
                                     TestLabel "4x4_SWAP[0][1]_Test3" test17,
                                     TestLabel "4x4_SWAP[0][1]_Test4" test18,
                                     TestLabel "4x4_CX[0][1]_Test1" test19,
                                     TestLabel "4x4_CX[0][1]_Test2" test20,
                                     TestLabel "4x4_CX[0][1]_Test3" test21,
                                     TestLabel "4x4_CX[0][1]_Test4" test22,
                                     TestLabel "4x4_CX[1][0]_Test1" test23,
                                     TestLabel "4x4_CX[1][0]_Test2" test24,
                                     TestLabel "4x4_CX[1][0]_Test3" test25,
                                     TestLabel "4x4_CX[1][0]_Test4" test26,
                                     TestLabel "4x4_CZ[0][1]_Is_CZ[1][0]" test27,
                                     TestLabel "4x4_CZ[0][1]_Test1" test28,
                                     TestLabel "4x4_CZ[0][1]_Test2" test29,
                                     TestLabel "4x4_CZ[0][1]_Test3" test30,
                                     TestLabel "4x4_CZ[0][1]_Test4" test31,
                                     TestLabel "4x4_K[0][1]_Is_H_Tensor_H" test32,
                                     TestLabel "4x4_K[1][0]_Is_H_Tensor_H" test33]

main = defaultMain tests
