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

test13 = TestCase (assertEqual "Checks SWP[0][1] = SWP."
                              gate_swap gate_swap01)
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test14 = TestCase (assertEqual "Checks SWP[1][0] = SWP."
                              gate_swap gate_swap10)
         where gate_swap10 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap BotBit)

test15 = TestCase (assertEqual "Checks SWP[0][1] sends |00> to |00>."
                              state_00 (gate_swap01 .*. state_00))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test16 = TestCase (assertEqual "Checks SWP[0][1] sends |01> to |10>."
                              state_10 (gate_swap01 .*. state_01))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test17 = TestCase (assertEqual "Checks SWP[0][1] sends |10> to |01>."
                              state_01 (gate_swap01 .*. state_10))
         where gate_swap01 = prepare_gate_4x4 (TwoQubitOp4x4 gate_swap TopBit)

test18 = TestCase (assertEqual "Checks SWP[0][1] sends |11> to |11>."
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
-- Tests the implementation of the tree-qubit templates.

state_000 = state_00 `tensor` state_0
state_001 = state_00 `tensor` state_1
state_010 = state_01 `tensor` state_0
state_011 = state_01 `tensor` state_1
state_100 = state_10 `tensor` state_0
state_101 = state_10 `tensor` state_1
state_110 = state_11 `tensor` state_0
state_111 = state_11 `tensor` state_1

test34 = TestCase (assertEqual "Checks X[0] is gate_x `tensor` gate_id `tensor` gate_id."
                               (gate_x `tensor` gate_id `tensor` gate_id) gate_x0)
         where gate_x0 = prepare_gate_8x8 (OneQubitOp8x8 gate_x LBit)

test35 = TestCase (assertEqual "Checks X[1] is gate_id `tensor` gate_x `tensor` gate_id."
                               (gate_id `tensor` gate_x `tensor` gate_id) gate_x1)
         where gate_x1 = prepare_gate_8x8 (OneQubitOp8x8 gate_x MBit)

test36 = TestCase (assertEqual "Checks X[2] is gate_id `tensor` gate_id `tensor` gate_x."
                               (gate_id `tensor` gate_id `tensor` gate_x) gate_x2)
         where gate_x2 = prepare_gate_8x8 (OneQubitOp8x8 gate_x RBit)

test37 = TestCase (assertEqual "Checks CX[0][1] is gate_cx `tensor` gate_id."
                               (gate_cx `tensor` gate_id) actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gate_cx LBit TopBit)

test38 = TestCase (assertEqual "Checks CX[0][2] is SWP[1][2].CX[0][1].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gate_cx LBit BotBit)
               gate_swap12 = (gate_id `tensor` gate_swap)
               expected_gate = gate_swap12 * (gate_cx `tensor` gate_id) * gate_swap12

test39 = TestCase (assertEqual "Checks CX[1][0] is SWP[0][1].CX[0][1].SWP[0][1]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gate_cx MBit TopBit)
               expected_gate = (gate_swap * gate_cx * gate_swap) `tensor` gate_id

test40 = TestCase (assertEqual "Checks CX[1][2] is gate_id `tensor` gate_cx."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gate_cx MBit BotBit)
               expected_gate = gate_id `tensor` gate_cx

test41 = TestCase (assertEqual "Checks CX[2][0] is SWP[1][2].CX[1][0].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gate_cx RBit TopBit)
               gate_cx10 = (gate_swap * gate_cx * gate_swap) `tensor` gate_id
               gate_swap12 = gate_id `tensor` gate_swap
               expected_gate = gate_swap12 * gate_cx10 * gate_swap12

test42 = TestCase (assertEqual "Checks CX[2][1] is gate_id `tensor` CX[1][0]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gate_cx RBit BotBit)
               expected_gate = gate_id `tensor` (gate_swap * gate_cx * gate_swap)

test43 = TestCase (assertEqual "Checks CCX sends |000> to |000>."
                               state_000 (gate_tof .*. state_000))

test44 = TestCase (assertEqual "Checks CCX sends |001> to |001>."
                               state_001 (gate_tof .*. state_001))

test45 = TestCase (assertEqual "Checks CCX sends |010> to |010>."
                               state_010 (gate_tof .*. state_010))

test46 = TestCase (assertEqual "Checks CCX sends |011> to |011>."
                               state_011 (gate_tof .*. state_011))

test47 = TestCase (assertEqual "Checks CCX sends |100> to |100>."
                               state_100 (gate_tof .*. state_100))

test48 = TestCase (assertEqual "Checks CCX sends |101> to |101>."
                               state_101 (gate_tof .*. state_101))

test49 = TestCase (assertEqual "Checks CCX sends |110> to |111>."
                               state_111 (gate_tof .*. state_110))

test50 = TestCase (assertEqual "Checks CCX sends |111> to |110>."
                               state_110 (gate_tof .*. state_111))

-- The Tofolli gate is invariant under swapping the two controls. This gate is not.
asym_gate = (gate_id `tensor` gate_x `tensor` gate_id) * gate_tof

test51 = TestCase (assertEqual "Checks GATE[0][1][2] is GATE."
                               asym_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate LBit TopBit)

test52 = TestCase (assertEqual "Checks GATE[0][1][2] is SWP[1][2].GATE.SWP[1][2]"
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate LBit BotBit)
               gate_swap12 = gate_id `tensor` gate_swap
               expected_gate = gate_swap12 * asym_gate * gate_swap12

test53 = TestCase (assertEqual "Checks GATE[1][0][2] is SWP[0][1].GATE.SWP[0][1]"
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate MBit TopBit)
               gate_swap01 = gate_swap `tensor` gate_id
               expected_gate = gate_swap01 * asym_gate * gate_swap01

test54 = TestCase (assertEqual "Checks GATE[1][2][0] with SWP[0][1].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate MBit BotBit)
               gate_swap01 = gate_swap `tensor` gate_id
               gate_swap12 = gate_id `tensor` gate_swap
               gate_lhs = gate_swap01 * gate_swap12
               gate_rhs = gate_swap12 * gate_swap01
               expected_gate = gate_lhs * asym_gate * gate_rhs

test55 = TestCase (assertEqual "Checks GATE[2][0][1] with SWP[1][2].SWP[0][1]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate RBit TopBit)
               gate_swap01 = gate_swap `tensor` gate_id
               gate_swap12 = gate_id `tensor` gate_swap
               gate_lhs = gate_swap12 * gate_swap01
               gate_rhs = gate_swap01 * gate_swap12
               expected_gate = gate_lhs * asym_gate * gate_rhs

test56 = TestCase (assertEqual "Checks GATE[2][1][0] with SWP[1][2].SWP[0][1].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate RBit BotBit)
               gate_swap01 = gate_swap `tensor` gate_id
               gate_swap12 = gate_id `tensor` gate_swap
               gate_lhs = gate_swap12 * gate_swap01 * gate_swap12
               gate_rhs = gate_swap12 * gate_swap01 * gate_swap12
               expected_gate = gate_lhs * asym_gate * gate_rhs

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
                                     TestLabel "4x4_SWP[0][1]_MatrixTest" test13,
                                     TestLabel "4x4_SWP[0][1]_Is_SWP[1][0]" test14,
                                     TestLabel "4x4_SWP[0][1]_Test1" test15,
                                     TestLabel "4x4_SWP[0][1]_Test2" test16,
                                     TestLabel "4x4_SWP[0][1]_Test3" test17,
                                     TestLabel "4x4_SWP[0][1]_Test4" test18,
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
                                     TestLabel "4x4_K[1][0]_Is_H_Tensor_H" test33,
                                     TestLabel "8x8_X[0]_Is_X_Tensor_1_Tensor_1" test34,
                                     TestLabel "8x8_X[0]_Is_1_Tensor_X_Tensor_1" test35,
                                     TestLabel "8x8_X[0]_Is_1_Tensor_1_Tensor_X" test36,
                                     TestLabel "8x8_CX[0][1]_Is_CX_Tensor_1" test37,
                                     TestLabel "8x8_CX[0][2]_Matrix_Check" test38,
                                     TestLabel "8x8_CX[1][0]_Matrix_Check" test39,
                                     TestLabel "8x8_CX[1][2]_Is_1_Tensor_CX" test40,
                                     TestLabel "8x8_CX[2][0]_Matrix_Check" test41,
                                     TestLabel "8x8_CX[2][1]_Matrix_Check" test42,
                                     TestLabel "8x8_Tof_Test1" test43,
                                     TestLabel "8x8_Tof_Test2" test44,
                                     TestLabel "8x8_Tof_Test3" test45,
                                     TestLabel "8x8_Tof_Test4" test46,
                                     TestLabel "8x8_Tof_Test5" test47,
                                     TestLabel "8x8_Tof_Test6" test48,
                                     TestLabel "8x8_Tof_Test7" test49,
                                     TestLabel "8x8_Tof_Test8" test50,
                                     TestLabel "8x8_GATE[0][1][2]_Is_GATE" test51,
                                     TestLabel "8x8_GATE[0][2][1]_Matrix_Check" test52,
                                     TestLabel "8x8_GATE[1][0][2]_Matrix_Check" test53,
                                     TestLabel "8x8_GATE[1][2][0]_Matrix_Check" test54,
                                     TestLabel "8x8_GATE[2][0][1]_Matrix_Check" test55,
                                     TestLabel "8x8_GATE[2][1][0]_Matrix_Check" test56]

main = defaultMain tests
