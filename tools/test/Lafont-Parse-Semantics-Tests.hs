module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quantum.Synthesis.Matrix
import Lafont.Generators.QubitGates
import Lafont.Parse.Semantics

-----------------------------------------------------------------------------------------
-- Quantum Operator Semantics Parsing.

x0_4x4 = gateX `tensor` gateId
x1_4x4 = gateId `tensor` gateX
z0_4x4 = gateZ `tensor` gateId
z1_4x4 = gateId `tensor` gateZ
cx10_4x4 = gateSwap * gateCX * gateSwap

test1 = TestCase (assertEqual "Can interpret the empty string as a 2 qubit circuit."
                              (Right (gateId `tensor` gateId))
                              (interpret2QubitCliffordDTofGate "  ε  -- test"))

test2 = TestCase (assertEqual "Can interpret X[0] as a 2 qubit circuit."
                              (Right x0_4x4)
                              (interpret2QubitCliffordDTofGate "X[0]"))

test3 = TestCase (assertEqual "Can interpret X[1] as a 2 qubit circuit."
                              (Right x1_4x4)
                              (interpret2QubitCliffordDTofGate "X[1]"))

test4 = TestCase (assertEqual "Can interpret Z[0] as a 2 qubit circuit."
                              (Right z0_4x4)
                              (interpret2QubitCliffordDTofGate "Z[0]"))

test5 = TestCase (assertEqual "Can interpret Z[1] as a 2 qubit circuit."
                              (Right z1_4x4)
                              (interpret2QubitCliffordDTofGate "Z[1]"))

test6 = TestCase (assertEqual "Can interpret CZ as a 2 qubit circuit."
                              (Right gateCZ)
                              (interpret2QubitCliffordDTofGate "CZ"))

test7 = TestCase (assertEqual "Can interpret K as a 2 qubit circuit."
                              (Right gateK)
                              (interpret2QubitCliffordDTofGate "K"))

test8 = TestCase (assertEqual "Can interpret SWAP as a 2 qubit circuit."
                              (Right gateSwap)
                              (interpret2QubitCliffordDTofGate "SWAP"))

test9 = TestCase (assertEqual "Can interpret CX[0][1] as a 2 qubit circuit."
                              (Right gateCX)
                              (interpret2QubitCliffordDTofGate "CX[0][1]"))

test10 = TestCase (assertEqual "Can interpret CX[1][0] as a 2 qubit circuit."
                               (Right cx10_4x4)
                               (interpret2QubitCliffordDTofGate "CX[1][0]"))

test11 = TestCase (assertEqual "Can interpret long strings as a 2 qubit circuit (1/2)."
                               (Right gate)
                               (interpret2QubitCliffordDTofGate word))
    where word = "SWAP.CX[1][0].X[0].Z[1].K.CZ.SWAP.SWAP"
          gate = gateSwap * cx10_4x4 * x0_4x4 * z1_4x4 * gateK * gateCZ

test12 = TestCase (assertEqual "Can interpret long strings as a 2 qubit circuit (2/2)."
                               (Right gate)
                               (interpret2QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].K.Z[0].Z[1].X[0].X[1]"
          pref = x0_4x4 * x1_4x4 * z0_4x4 * z1_4x4
          invs = z0_4x4 * z1_4x4 * x0_4x4 * x1_4x4
          gate = pref * gateK * invs

test13 = TestCase (assertEqual "Unknown operators rejected for 2 qubit circuits (1/2)."
                               (Left "Unknown two qubit operator: MadeUp")
                               (interpret2QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].MadeUp.Z[0].Z[1].X[0].X[1]"

test14 = TestCase (assertEqual "Unknown operators rejected for 2 qubit circuits (2/2)."
                               (Left "Unknown one qubit operator: MadeUp")
                               (interpret2QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].MadeUp[0].Z[0].Z[1].X[0].X[1]"

test15 = TestCase (assertEqual "OOB arguments rejected for 2 qubit circuits (1/2)."
                               (Left "Invalid gate position: Z[5]")
                               (interpret2QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[5].K.Z[0].Z[1].X[0].X[1]"

test16 = TestCase (assertEqual "OOB arguments rejected for 2 qubit circuits (2/2)."
                               (Left "Invalid gate position: CCX[1][1]")
                               (interpret2QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].CCX[1][1].Z[0].Z[1].X[0].X[1]"

test17 = TestCase (assertEqual "Interpretation framework rejects non-words."
                               (Left "Expected a word")
                               (interpret2QubitCliffordDTofGate word))
    where word = "54 X[0].X[1].Z[0].Z[1].K.Z[0].Z[1].X[0].X[1]"

test18 = TestCase (assertEqual "Interpretation framework requires a single word."
                               (Left "Expected a single word")
                               (interpret2QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].K.Z[0].Z[1].X[0].X[1] 54"

x0_8x8 = gateX `tensor` gateId `tensor` gateId
x1_8x8 = gateId `tensor` gateX `tensor` gateId
x2_8x8 = gateId `tensor` gateId `tensor` gateX
z0_8x8 = gateZ `tensor` gateId `tensor` gateId
z1_8x8 = gateId `tensor` gateZ `tensor` gateId
z2_8x8 = gateId `tensor` gateId `tensor` gateZ
swap01_8x8 = gateSwap `tensor` gateId
swap10_8x8 = gateSwap `tensor` gateId
swap12_8x8 = gateId `tensor` gateSwap
swap21_8x8 = gateId `tensor` gateSwap
swap02_8x8 = swap12_8x8 * swap01_8x8 * swap21_8x8
swap20_8x8 = swap01_8x8 * swap21_8x8 * swap10_8x8
cx01_8x8 = gateCX `tensor` gateId
cx10_8x8 = cx10_4x4 `tensor` gateId
cx12_8x8 = gateId `tensor` gateCX
cx21_8x8 = gateId `tensor` cx10_4x4
cx02_8x8 = swap12_8x8 * cx01_8x8 * swap21_8x8
cx20_8x8 = swap01_8x8 * cx21_8x8 * swap01_8x8
cz01_8x8 = gateCZ `tensor` gateId
cz10_8x8 = gateCZ `tensor` gateId
cz12_8x8 = gateId `tensor` gateCZ
cz21_8x8 = gateId `tensor` gateCZ
cz02_8x8 = swap12_8x8 * cz01_8x8 * swap21_8x8
cz20_8x8 = swap01_8x8 * cz21_8x8 * swap10_8x8
k01_8x8 = gateK `tensor` gateId
k10_8x8 = gateK `tensor` gateId
k12_8x8 = gateId `tensor` gateK
k21_8x8 = gateId `tensor` gateK
k02_8x8 = swap12_8x8 * k01_8x8 * swap21_8x8
k20_8x8 = swap01_8x8 * k21_8x8 * swap10_8x8
ccx012_8x8 = gateTof
ccx021_8x8 = swap12_8x8 * gateTof * swap21_8x8
ccx102_8x8 = swap01_8x8 * gateTof * swap10_8x8
ccx120_8x8 = swap02_8x8 * ccx102_8x8 * swap20_8x8
ccx201_8x8 = swap12_8x8 * ccx102_8x8 * swap21_8x8
ccx210_8x8 = swap01_8x8 * ccx201_8x8 * swap10_8x8

test19 = TestCase (assertEqual "Can interpret the empty string as a 3 qubit circuit."
                               (Right (gateId `tensor` gateId `tensor` gateId))
                               (interpret3QubitCliffordDTofGate "  ε  -- test"))

test20 = TestCase (assertEqual "Can interpret X[0] as a 3 qubit circuit."
                               (Right x0_8x8)
                               (interpret3QubitCliffordDTofGate "X[0]"))

test21 = TestCase (assertEqual "Can interpret X[1] as a 3 qubit circuit."
                               (Right x1_8x8)
                               (interpret3QubitCliffordDTofGate "X[1]"))

test22 = TestCase (assertEqual "Can interpret X[2] as a 3 qubit circuit."
                               (Right x2_8x8)
                               (interpret3QubitCliffordDTofGate "X[2]"))

test23 = TestCase (assertEqual "Can interpret Z[0] as a 3 qubit circuit."
                               (Right z0_8x8)
                               (interpret3QubitCliffordDTofGate "Z[0]"))

test24 = TestCase (assertEqual "Can interpret Z[1] as a 3 qubit circuit."
                               (Right z1_8x8)
                               (interpret3QubitCliffordDTofGate "Z[1]"))

test25 = TestCase (assertEqual "Can interpret Z[2] as a 3 qubit circuit."
                               (Right z2_8x8)
                               (interpret3QubitCliffordDTofGate "Z[2]"))

test26 = TestCase (assertEqual "Can interpret SWAP[0][1] as a 3 qubit circuit."
                               (Right swap01_8x8)
                               (interpret3QubitCliffordDTofGate "SWAP[0][1]"))

test27 = TestCase (assertEqual "Can interpret SWAP[0][2] as a 3 qubit circuit."
                               (Right swap02_8x8)
                               (interpret3QubitCliffordDTofGate "SWAP[0][2]"))

test28 = TestCase (assertEqual "Can interpret SWAP[1][0] as a 3 qubit circuit."
                               (Right swap10_8x8)
                               (interpret3QubitCliffordDTofGate "SWAP[1][0]"))

test29 = TestCase (assertEqual "Can interpret SWAP[1][2] as a 3 qubit circuit."
                               (Right swap12_8x8)
                               (interpret3QubitCliffordDTofGate "SWAP[1][2]"))

test30 = TestCase (assertEqual "Can interpret SWAP[2][0] as a 3 qubit circuit."
                               (Right swap20_8x8)
                               (interpret3QubitCliffordDTofGate "SWAP[2][0]"))

test31 = TestCase (assertEqual "Can interpret SWAP[2][1] as a 3 qubit circuit."
                               (Right swap21_8x8)
                               (interpret3QubitCliffordDTofGate "SWAP[2][1]"))

test32 = TestCase (assertEqual "Can interpret CX[0][1] as a 3 qubit circuit."
                               (Right cx01_8x8)
                               (interpret3QubitCliffordDTofGate "CX[0][1]"))

test33 = TestCase (assertEqual "Can interpret CX[0][2] as a 3 qubit circuit."
                               (Right cx02_8x8)
                               (interpret3QubitCliffordDTofGate "CX[0][2]"))

test34 = TestCase (assertEqual "Can interpret CX[1][0] as a 3 qubit circuit."
                               (Right cx10_8x8)
                               (interpret3QubitCliffordDTofGate "CX[1][0]"))

test35 = TestCase (assertEqual "Can interpret CX[1][2] as a 3 qubit circuit."
                               (Right cx12_8x8)
                               (interpret3QubitCliffordDTofGate "CX[1][2]"))

test36 = TestCase (assertEqual "Can interpret CX[2][0] as a 3 qubit circuit."
                               (Right cx20_8x8)
                               (interpret3QubitCliffordDTofGate "CX[2][0]"))

test37 = TestCase (assertEqual "Can interpret CX[2][1] as a 3 qubit circuit."
                               (Right cx21_8x8)
                               (interpret3QubitCliffordDTofGate "CX[2][1]"))

test38 = TestCase (assertEqual "Can interpret CZ[0][1] as a 3 qubit circuit."
                               (Right cz01_8x8)
                               (interpret3QubitCliffordDTofGate "CZ[0][1]"))

test39 = TestCase (assertEqual "Can interpret CZ[0][2] as a 3 qubit circuit."
                               (Right cz02_8x8)
                               (interpret3QubitCliffordDTofGate "CZ[0][2]"))

test40 = TestCase (assertEqual "Can interpret CZ[1][0] as a 3 qubit circuit."
                               (Right cz10_8x8)
                               (interpret3QubitCliffordDTofGate "CZ[1][0]"))

test41 = TestCase (assertEqual "Can interpret CZ[1][2] as a 3 qubit circuit."
                               (Right cz12_8x8)
                               (interpret3QubitCliffordDTofGate "CZ[1][2]"))

test42 = TestCase (assertEqual "Can interpret CZ[2][0] as a 3 qubit circuit."
                               (Right cz20_8x8)
                               (interpret3QubitCliffordDTofGate "CZ[2][0]"))

test43 = TestCase (assertEqual "Can interpret CZ[2][1] as a 3 qubit circuit."
                               (Right cz21_8x8)
                               (interpret3QubitCliffordDTofGate "CZ[2][1]"))

test44 = TestCase (assertEqual "Can interpret K[0][1] as a 3 qubit circuit."
                               (Right k01_8x8)
                               (interpret3QubitCliffordDTofGate "K[0][1]"))

test45 = TestCase (assertEqual "Can interpret K[0][2] as a 3 qubit circuit."
                               (Right k02_8x8)
                               (interpret3QubitCliffordDTofGate "K[0][2]"))

test46 = TestCase (assertEqual "Can interpret K[1][0] as a 3 qubit circuit."
                               (Right k10_8x8)
                               (interpret3QubitCliffordDTofGate "K[1][0]"))

test47 = TestCase (assertEqual "Can interpret K[1][2] as a 3 qubit circuit."
                               (Right k12_8x8)
                               (interpret3QubitCliffordDTofGate "K[1][2]"))

test48 = TestCase (assertEqual "Can interpret K[2][0] as a 3 qubit circuit."
                               (Right k20_8x8)
                               (interpret3QubitCliffordDTofGate "K[2][0]"))

test49 = TestCase (assertEqual "Can interpret K[2][1] as a 3 qubit circuit."
                               (Right k21_8x8)
                               (interpret3QubitCliffordDTofGate "K[2][1]"))

test50 = TestCase (assertEqual "Can interpret CCX[0][1][2] as a 3 qubit circuit."
                               (Right ccx012_8x8)
                               (interpret3QubitCliffordDTofGate "CCX[0][1][2]"))

test51 = TestCase (assertEqual "Can interpret CCX[0][2][1] as a 3 qubit circuit."
                               (Right ccx021_8x8)
                               (interpret3QubitCliffordDTofGate "CCX[0][2][1]"))

test52 = TestCase (assertEqual "Can interpret CCX[1][0][2] as a 3 qubit circuit."
                               (Right ccx102_8x8)
                               (interpret3QubitCliffordDTofGate "CCX[1][0][2]"))

test53 = TestCase (assertEqual "Can interpret CCX[1][2][0] as a 3 qubit circuit."
                               (Right ccx120_8x8)
                               (interpret3QubitCliffordDTofGate "CCX[1][2][0]"))

test54 = TestCase (assertEqual "Can interpret CCX[2][0][1] as a 3 qubit circuit."
                               (Right ccx201_8x8)
                               (interpret3QubitCliffordDTofGate "CCX[2][0][1]"))

test55 = TestCase (assertEqual "Can interpret CCX[2][1][0] as a 3 qubit circuit."
                               (Right ccx210_8x8)
                               (interpret3QubitCliffordDTofGate "CCX[2][1][0]"))

test56 = TestCase (assertEqual "Can interpret long strings as a 3 qubit circuit (1/2)."
                               (Right gate)
                               (interpret3QubitCliffordDTofGate word))
    where word = "CCX[0][1][2].CCX[2][1][0].SWAP[1][2].Z[1].CZ[0][2].CX[0][1]"
          gate = gateTof * ccx210_8x8 * swap12_8x8 * z1_8x8 * cz02_8x8 * cx01_8x8

test57 = TestCase (assertEqual "Can interpret long strings as a 3 qubit circuit (2/2)."
                               (Right gate)
                               (interpret3QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].K[0][1].Z[0].Z[1].X[0].X[1]"
          pref = x0_8x8 * x1_8x8 * z0_8x8 * z1_8x8
          invs = z0_8x8 * z1_8x8 * x0_8x8 * x1_8x8
          gate = pref * k01_8x8 * invs

test58 = TestCase (assertEqual "Unknown operators rejected for 3 qubit circuits (1/2)."
                               (Left "Invalid argument count in: MadeUp")
                               (interpret3QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].MadeUp.Z[0].Z[1].X[0].X[1]"

test59 = TestCase (assertEqual "Unknown operators rejected for 3 qubit circuits (2/2)."
                               (Left "Unknown one qubit operator: MadeUp")
                               (interpret3QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].MadeUp[0].Z[0].Z[1].X[0].X[1]"

test60 = TestCase (assertEqual "OOB arguments rejected for 3 qubit circuits (1/2)."
                               (Left "Invalid gate position: Z[5]")
                               (interpret3QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[5].K[0][1].Z[0].Z[1].X[0].X[1]"

test61 = TestCase (assertEqual "OOB arguments rejected for 2 qubit circuits (2/2)."
                               (Left "Invalid gate position: CCCX[0][1][1]")
                               (interpret3QubitCliffordDTofGate word))
    where word = "X[0].X[1].Z[0].Z[1].CCCX[0][1][1].Z[0].Z[1].X[0].X[1]"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "ParseSem_2Qubit_ID" test1,
                                     TestLabel "ParseSem_2Qubit_X[0]" test2,
                                     TestLabel "ParseSem_2Qubit_X[1]" test3,
                                     TestLabel "ParseSem_2Qubit_Z[0]" test4,
                                     TestLabel "ParseSem_2Qubit_Z[1]" test5,
                                     TestLabel "ParseSem_2Qubit_CZ" test6,
                                     TestLabel "ParseSem_2Qubit_K" test7,
                                     TestLabel "ParseSem_2Qubit_SWAP" test8,
                                     TestLabel "ParseSem_2Qubit_CX[0][1]" test9,
                                     TestLabel "ParseSem_2Qubit_CX[1][0]" test10,
                                     TestLabel "ParseSem_2Qubit_Long1" test11,
                                     TestLabel "ParseSem_2Qubit_Long2" test12,
                                     TestLabel "ParseSem_2Qubit_UknOp1" test13,
                                     TestLabel "ParseSem_2Qubit_UknOp2" test14,
                                     TestLabel "ParseSem_2Qubit_OOBArg1" test15,
                                     TestLabel "ParseSem_2Qubit_OOBArg2" test16,
                                     TestLabel "ParseSem_NonWord" test17,
                                     TestLabel "ParseSem_ManyWords" test18,
                                     TestLabel "ParseSem_3Qubit_ID" test19,
                                     TestLabel "ParseSem_3Qubit_X[0]" test20,
                                     TestLabel "ParseSem_3Qubit_X[1]" test21,
                                     TestLabel "ParseSem_3Qubit_X[2]" test22,
                                     TestLabel "ParseSem_3Qubit_Z[0]" test23,
                                     TestLabel "ParseSem_3Qubit_Z[1]" test24,
                                     TestLabel "ParseSem_3Qubit_Z[2]" test25,
                                     TestLabel "ParseSem_3Qubit_SWAP[0][1]" test26,
                                     TestLabel "ParseSem_3Qubit_SWAP[0][2]" test27,
                                     TestLabel "ParseSem_3Qubit_SWAP[1][0]" test28,
                                     TestLabel "ParseSem_3Qubit_SWAP[1][2]" test29,
                                     TestLabel "ParseSem_3Qubit_SWAP[2][0]" test30,
                                     TestLabel "ParseSem_3Qubit_SWAP[2][1]" test31,
                                     TestLabel "ParseSem_3Qubit_CX[0][1]" test32,
                                     TestLabel "ParseSem_3Qubit_CX[0][2]" test33,
                                     TestLabel "ParseSem_3Qubit_CX[1][0]" test34,
                                     TestLabel "ParseSem_3Qubit_CX[1][2]" test35,
                                     TestLabel "ParseSem_3Qubit_CX[2][0]" test36,
                                     TestLabel "ParseSem_3Qubit_CX[2][1]" test37,
                                     TestLabel "ParseSem_3Qubit_CZ[0][1]" test38,
                                     TestLabel "ParseSem_3Qubit_CZ[0][2]" test39,
                                     TestLabel "ParseSem_3Qubit_CZ[1][0]" test40,
                                     TestLabel "ParseSem_3Qubit_CZ[1][2]" test41,
                                     TestLabel "ParseSem_3Qubit_CZ[2][0]" test42,
                                     TestLabel "ParseSem_3Qubit_CZ[2][1]" test43,
                                     TestLabel "ParseSem_3Qubit_K[0][1]" test44,
                                     TestLabel "ParseSem_3Qubit_K[0][2]" test45,
                                     TestLabel "ParseSem_3Qubit_K[1][0]" test46,
                                     TestLabel "ParseSem_3Qubit_K[1][2]" test47,
                                     TestLabel "ParseSem_3Qubit_K[2][0]" test48,
                                     TestLabel "ParseSem_3Qubit_K[2][1]" test49,
                                     TestLabel "ParseSem_3Qubit_CCX[0][1][2]" test50,
                                     TestLabel "ParseSem_3Qubit_CCX[0][2][1]" test51,
                                     TestLabel "ParseSem_3Qubit_CCX[1][0][2]" test52,
                                     TestLabel "ParseSem_3Qubit_CCX[1][2][0]" test53,
                                     TestLabel "ParseSem_3Qubit_CCX[2][0][1]" test54,
                                     TestLabel "ParseSem_3Qubit_CCX[2][1][0]" test55,
                                     TestLabel "ParseSem_3Qubit_Long1" test56,
                                     TestLabel "ParseSem_3Qubit_Long2" test57,
                                     TestLabel "ParseSem_3Qubit_UknOp1" test58,
                                     TestLabel "ParseSem_3Qubit_UknOp2" test59,
                                     TestLabel "ParseSem_3Qubit_OOBArg1" test60,
                                     TestLabel "ParseSem_3Qubit_OOBArg2" test61]

main = defaultMain tests
