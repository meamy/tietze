module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Generators.QubitGates

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

test1 = TestCase (assertEqual "Checks gateId sends |0> to |0>."
                              state_0 (gateId .*. state_0))

test2 = TestCase (assertEqual "Checks gateId sends |1> to |1>."
                              state_1 (gateId .*. state_1))

test3 = TestCase (assertEqual "Checks gateX sends |0> to |1>."
                              state_1 (gateX .*. state_0))

test4 = TestCase (assertEqual "Checks gateX sends |1> to |0>."
                              state_0 (gateX .*. state_1))

test5 = TestCase (assertEqual "Checks gateY sends |0> to i|1>."
                              (drcPhase i state_1) (gateY .*. (drcPhase 1 state_0)))

test6 = TestCase (assertEqual "Checks gateY sends |1> to -i|0>."
                              (drcPhase (-i) state_0) (gateY .*. (drcPhase 1 state_1)))

test7 = TestCase (assertEqual "Checks gateX sends |0> to |0>."
                              state_0 (gateZ .*. state_0))

test8 = TestCase (assertEqual "Checks gateX sends |1> to -|1>."
                              (dyadPhase (-1) state_1) (gateZ .*. state_1))

test9 = TestCase (assertEqual "Checks gateH sends |0> to |+>."
                              state_plus (gateH .*. (drPhase 1 state_0)))

test10 = TestCase (assertEqual "Checks gateH sends |0> to |->."
                               state_minus (gateH .*. (drPhase 1 state_1)))

-----------------------------------------------------------------------------------------
-- Tests the implementation of the two-qubit templates.

state_00 = state_0 `tensor` state_0
state_01 = state_0 `tensor` state_1
state_10 = state_1 `tensor` state_0
state_11 = state_1 `tensor` state_1

test11 = TestCase (assertEqual "Checks X[0] is gateX `tensor` gateId."
                               (gateX `tensor` gateId) gateX0)
         where gateX0 = prepare_gate_4x4 (OneQubitOp4x4 gateX TopBit)

test12 = TestCase (assertEqual "Checks X[1] is gateId `tensor` gateX."
                               (gateId `tensor` gateX) gateX1)
         where gateX1 = prepare_gate_4x4 (OneQubitOp4x4 gateX BotBit)

test13 = TestCase (assertEqual "Checks SWP[0][1] = SWP."
                              gateSwap gateSwap01)
         where gateSwap01 = prepare_gate_4x4 (TwoQubitOp4x4 gateSwap TopBit)

test14 = TestCase (assertEqual "Checks SWP[1][0] = SWP."
                              gateSwap gateSwap10)
         where gateSwap10 = prepare_gate_4x4 (TwoQubitOp4x4 gateSwap BotBit)

test15 = TestCase (assertEqual "Checks SWP[0][1] sends |00> to |00>."
                              state_00 (gateSwap01 .*. state_00))
         where gateSwap01 = prepare_gate_4x4 (TwoQubitOp4x4 gateSwap TopBit)

test16 = TestCase (assertEqual "Checks SWP[0][1] sends |01> to |10>."
                              state_10 (gateSwap01 .*. state_01))
         where gateSwap01 = prepare_gate_4x4 (TwoQubitOp4x4 gateSwap TopBit)

test17 = TestCase (assertEqual "Checks SWP[0][1] sends |10> to |01>."
                              state_01 (gateSwap01 .*. state_10))
         where gateSwap01 = prepare_gate_4x4 (TwoQubitOp4x4 gateSwap TopBit)

test18 = TestCase (assertEqual "Checks SWP[0][1] sends |11> to |11>."
                              state_11 (gateSwap01 .*. state_11))
         where gateSwap01 = prepare_gate_4x4 (TwoQubitOp4x4 gateSwap TopBit)

test19 = TestCase (assertEqual "Checks CX[0][1] sends |00> to |00>."
                               state_00 (gateCX01 .*. state_00))
         where gateCX01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX TopBit)

test20 = TestCase (assertEqual "Checks CX[0][1] sends |01> to |01>."
                              state_01 (gateCX01 .*. state_01))
         where gateCX01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX TopBit)

test21 = TestCase (assertEqual "Checks CX[0][1] sends |10> to |11>."
                              state_11 (gateCX01 .*. state_10))
         where gateCX01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX TopBit)

test22 = TestCase (assertEqual "Checks CX[0][1] sends |11> to |10>."
                              state_10 (gateCX01 .*. state_11))
         where gateCX01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX TopBit)

test23 = TestCase (assertEqual "Checks CX[1][0] sends |00> to |00>."
                               state_00 (gateCX10 .*. state_00))
         where gateCX10 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX BotBit)

test24 = TestCase (assertEqual "Checks CX[1][0] sends |01> to |11>."
                              state_11 (gateCX10 .*. state_01))
         where gateCX10 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX BotBit)

test25 = TestCase (assertEqual "Checks CX[1][0] sends |10> to |10>."
                              state_10 (gateCX10 .*. state_10))
         where gateCX10 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX BotBit)

test26 = TestCase (assertEqual "Checks CX[0][1] sends |11> to |01>."
                              state_01 (gateCX10 .*. state_11))
         where gateCX10 = prepare_gate_4x4 (TwoQubitOp4x4 gateCX BotBit)

test27 = TestCase (assertEqual "Checks CZ[0][1] is CZ[1][0]."
                              gateCZ01 gateCZ10)
         where gateCZ01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCZ TopBit)
               gateCZ10 = prepare_gate_4x4 (TwoQubitOp4x4 gateCZ BotBit)

test28 = TestCase (assertEqual "Checks CZ[0][1] sends |00> to |00>."
                               state_00 (gateCZ01 .*. state_00))
         where gateCZ01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCZ TopBit)

test29 = TestCase (assertEqual "Checks CZ[0][1] sends |01> to |01>."
                               state_01 (gateCZ01 .*. state_01))
         where gateCZ01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCZ TopBit)

test30 = TestCase (assertEqual "Checks CZ[0][1] sends |10> to |10>."
                               state_10 (gateCZ01 .*. state_10))
         where gateCZ01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCZ TopBit)

test31 = TestCase (assertEqual "Checks CZ[0][1] sends |11> to -|11>."
                               ((-1) `scalarmult` state_11) (gateCZ01 .*. state_11))
         where gateCZ01 = prepare_gate_4x4 (TwoQubitOp4x4 gateCZ TopBit)

test32 = TestCase (assertEqual "Checks K[0][1] is H tensor H."
                               (gateH `tensor` gateH) gateK01)
         where typed_gate = gateK :: Unitary Four DRootTwo
               gateK01 = prepare_gate_4x4 (TwoQubitOp4x4 typed_gate TopBit)

test33 = TestCase (assertEqual "Checks K[1][0] is H tensor H."
                               (gateH `tensor` gateH) gateK10)
         where typed_gate = gateK :: Unitary Four DRootTwo
               gateK10 = prepare_gate_4x4 (TwoQubitOp4x4 typed_gate BotBit)

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

test34 = TestCase (assertEqual "Checks X[0] is gateX `tensor` gateId `tensor` gateId."
                               (gateX `tensor` gateId `tensor` gateId) gateX0)
         where gateX0 = prepare_gate_8x8 (OneQubitOp8x8 gateX LBit)

test35 = TestCase (assertEqual "Checks X[1] is gateId `tensor` gateX `tensor` gateId."
                               (gateId `tensor` gateX `tensor` gateId) gateX1)
         where gateX1 = prepare_gate_8x8 (OneQubitOp8x8 gateX MBit)

test36 = TestCase (assertEqual "Checks X[2] is gateId `tensor` gateId `tensor` gateX."
                               (gateId `tensor` gateId `tensor` gateX) gateX2)
         where gateX2 = prepare_gate_8x8 (OneQubitOp8x8 gateX RBit)

test37 = TestCase (assertEqual "Checks CX[0][1] is gateCX `tensor` gateId."
                               (gateCX `tensor` gateId) actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gateCX LBit TopBit)

test38 = TestCase (assertEqual "Checks CX[0][2] is SWP[1][2].CX[0][1].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gateCX LBit BotBit)
               gateSwap12 = (gateId `tensor` gateSwap)
               expected_gate = gateSwap12 * (gateCX `tensor` gateId) * gateSwap12

test39 = TestCase (assertEqual "Checks CX[1][0] is SWP[0][1].CX[0][1].SWP[0][1]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gateCX MBit TopBit)
               expected_gate = (gateSwap * gateCX * gateSwap) `tensor` gateId

test40 = TestCase (assertEqual "Checks CX[1][2] is gateId `tensor` gateCX."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gateCX MBit BotBit)
               expected_gate = gateId `tensor` gateCX

test41 = TestCase (assertEqual "Checks CX[2][0] is SWP[1][2].CX[1][0].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gateCX RBit TopBit)
               gateCX10 = (gateSwap * gateCX * gateSwap) `tensor` gateId
               gateSwap12 = gateId `tensor` gateSwap
               expected_gate = gateSwap12 * gateCX10 * gateSwap12

test42 = TestCase (assertEqual "Checks CX[2][1] is gateId `tensor` CX[1][0]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (TwoQubitOp8x8 gateCX RBit BotBit)
               expected_gate = gateId `tensor` (gateSwap * gateCX * gateSwap)

test43 = TestCase (assertEqual "Checks CCX sends |000> to |000>."
                               state_000 (gateTof .*. state_000))

test44 = TestCase (assertEqual "Checks CCX sends |001> to |001>."
                               state_001 (gateTof .*. state_001))

test45 = TestCase (assertEqual "Checks CCX sends |010> to |010>."
                               state_010 (gateTof .*. state_010))

test46 = TestCase (assertEqual "Checks CCX sends |011> to |011>."
                               state_011 (gateTof .*. state_011))

test47 = TestCase (assertEqual "Checks CCX sends |100> to |100>."
                               state_100 (gateTof .*. state_100))

test48 = TestCase (assertEqual "Checks CCX sends |101> to |101>."
                               state_101 (gateTof .*. state_101))

test49 = TestCase (assertEqual "Checks CCX sends |110> to |111>."
                               state_111 (gateTof .*. state_110))

test50 = TestCase (assertEqual "Checks CCX sends |111> to |110>."
                               state_110 (gateTof .*. state_111))

-- The Tofolli gate is invariant under swapping the two controls. This gate is not.
asym_gate = (gateId `tensor` gateX `tensor` gateId) * gateTof

test51 = TestCase (assertEqual "Checks GATE[0][1][2] is GATE."
                               asym_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate LBit TopBit)

test52 = TestCase (assertEqual "Checks GATE[0][1][2] is SWP[1][2].GATE.SWP[1][2]"
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate LBit BotBit)
               gateSwap12 = gateId `tensor` gateSwap
               expected_gate = gateSwap12 * asym_gate * gateSwap12

test53 = TestCase (assertEqual "Checks GATE[1][0][2] is SWP[0][1].GATE.SWP[0][1]"
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate MBit TopBit)
               gateSwap01 = gateSwap `tensor` gateId
               expected_gate = gateSwap01 * asym_gate * gateSwap01

test54 = TestCase (assertEqual "Checks GATE[1][2][0] with SWP[0][1].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate MBit BotBit)
               gateSwap01 = gateSwap `tensor` gateId
               gateSwap12 = gateId `tensor` gateSwap
               gate_lhs = gateSwap01 * gateSwap12
               gate_rhs = gateSwap12 * gateSwap01
               expected_gate = gate_lhs * asym_gate * gate_rhs

test55 = TestCase (assertEqual "Checks GATE[2][0][1] with SWP[1][2].SWP[0][1]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate RBit TopBit)
               gateSwap01 = gateSwap `tensor` gateId
               gateSwap12 = gateId `tensor` gateSwap
               gate_lhs = gateSwap12 * gateSwap01
               gate_rhs = gateSwap01 * gateSwap12
               expected_gate = gate_lhs * asym_gate * gate_rhs

test56 = TestCase (assertEqual "Checks GATE[2][1][0] with SWP[1][2].SWP[0][1].SWP[1][2]."
                               expected_gate actual_gate)
         where actual_gate = prepare_gate_8x8 (ThreeQubitOp8x8 asym_gate RBit BotBit)
               gateSwap01 = gateSwap `tensor` gateId
               gateSwap12 = gateId `tensor` gateSwap
               gate_lhs = gateSwap12 * gateSwap01 * gateSwap12
               gate_rhs = gateSwap12 * gateSwap01 * gateSwap12
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
