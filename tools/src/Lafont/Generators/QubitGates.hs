-- | A collection of quantum gate primitives to describe quantum circuit semantics.

module Lafont.Generators.QubitGates where

import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Quantum.Synthesis.CliffordT

-----------------------------------------------------------------------------------------
-- * Template specification types.

-- | Distinguishes a pair of qubits as top and bottom (as in a pair of horizontal wires).
data TwoBitPos = TopBit | BotBit

-- | Distinguishes a tiple of qubits as left, middle, and right (as in a tiple of
-- vertical wires).
data ThreeBitPos = LBit | MBit | RBit

-----------------------------------------------------------------------------------------
-- * One-Qubit Gates.

-- | The 2x2 identity matrix.
gate_id :: (Num a) => Matrix Two Two a
gate_id = matrix2x2 (1, 0)
                    (0, 1)

-- | The Pauli X gate.
gate_x :: (Num a) => Matrix Two Two a
gate_x = u2_X

-- | The Pauli Y gate.
gate_y :: (ComplexRing a) => Matrix Two Two a
gate_y = u2_Y

-- | The Pauli Z gate.
gate_z :: (Num a) => Matrix Two Two a
gate_z = u2_Z

-- | The Hadamard gate.
gate_h :: (RootHalfRing a) => Matrix Two Two a
gate_h = u2_H

-----------------------------------------------------------------------------------------
-- * Two-Qubit Gate Templates.

-- | The (unique) two qubit swap gate.
gate_swap :: (Num a) => Matrix Four Four a
gate_swap = swap

-- | The controlled X gate with control on qubit 0.
gate_cx :: (Num a) => Matrix Four Four a
gate_cx = cnot

-- | The controlled Z gate.
gate_cz :: (Num a) => Matrix Four Four a
gate_cz = matrix4x4 (1,0,0, 0)
                    (0,1,0, 0)
                    (0,0,1, 0)
                    (0,0,0,-1)

-- | The tensor of two Hadamard gates.
gate_k :: (HalfRing a) => Matrix Four Four a
gate_k = half `scalarmult` matrix4x4 (1, 1, 1, 1)
                                     (1,-1, 1,-1)
                                     (1, 1,-1,-1)
                                     (1,-1,-1, 1)

-- | A template for two qubit gates that implement given operators. For a one qubit
-- operation, the position specifies which qubit the gate is applied to. For a two qubit
-- gate, the position specifies which qubit is the primary input (e.g., a control).
data TwoQubitGate a = OneQubitOp4x4 (Matrix Two Two a) TwoBitPos
                    | TwoQubitOp4x4 (Matrix Four Four a) TwoBitPos

-- | Computes the 4x4 matrice of the two qubit gate described by a template.
prepare_gate_4x4 :: (Ring a) => TwoQubitGate a -> Matrix Four Four a
prepare_gate_4x4 (OneQubitOp4x4 m TopBit) = m `tensor` gate_id
prepare_gate_4x4 (OneQubitOp4x4 m BotBit) = gate_id `tensor` m
prepare_gate_4x4 (TwoQubitOp4x4 m TopBit) = m
prepare_gate_4x4 (TwoQubitOp4x4 m BotBit) = gate_swap * m * gate_swap

-----------------------------------------------------------------------------------------
-- * Three-Qubit Gate Templates.

-- | The doubly controlled X gate with controls on qubits 0 and 1. Otherwise known as a
-- Toffoli gate.
gate_tof :: (Num a) => Matrix Eight Eight a
gate_tof = matrix_of_rows [[1,0,0,0,0,0,0,0],
                           [0,1,0,0,0,0,0,0],
                           [0,0,1,0,0,0,0,0],
                           [0,0,0,1,0,0,0,0],
                           [0,0,0,0,1,0,0,0],
                           [0,0,0,0,0,1,0,0],
                           [0,0,0,0,0,0,0,1],
                           [0,0,0,0,0,0,1,0]]

-- | A template for three qubit gates that implement given operators. For a one qubit
-- operation, the position specifies which qubit the gate is applied to. For two and
-- three qubit gates, the positions specify the primary and secondary input. The primary
-- input (e.g. a control) is any of the three inputs. The secondary input (e.g., the
-- bit controlled by a two qubit operation, or the second control in a three qubit
-- operation) is any of the two remaining inputs.
data ThreeQubitGate a = OneQubitOp8x8 (Matrix Two Two a) ThreeBitPos
                      | TwoQubitOp8x8 (Matrix Four Four a) ThreeBitPos TwoBitPos
                      | ThreeQubitOp8x8 (Matrix Eight Eight a) ThreeBitPos TwoBitPos

-- | Computes the 8x8 matrice of the three qubit gate described by a template.
prepare_gate_8x8 :: (Ring a) => ThreeQubitGate a -> Matrix Eight Eight a
-- GATE(8x8)[0] := GATE(4x4)[0] `tensor` ID
prepare_gate_8x8 (OneQubitOp8x8 m LBit) = gate_4x4 `tensor` gate_id
    where gate_4x4 = prepare_gate_4x4 (OneQubitOp4x4 m TopBit)
-- GATE(8x8)[1] := GATE(4x4)[1] `tensor` ID
prepare_gate_8x8 (OneQubitOp8x8 m MBit) = gate_4x4 `tensor` gate_id
    where gate_4x4 = prepare_gate_4x4 (OneQubitOp4x4 m BotBit)
-- GATE(8x8)[2] := ID `tensor` GATE(4x4)[1]
prepare_gate_8x8 (OneQubitOp8x8 m RBit) = gate_id `tensor` gate_4x4
    where gate_4x4 = prepare_gate_4x4 (OneQubitOp4x4 m BotBit)
-- GATE(8x8)[0][1] := GATE(4x4)[0][1] `tensor` ID
prepare_gate_8x8 (TwoQubitOp8x8 m LBit TopBit) = gate_4x4 `tensor` gate_id
    where gate_4x4 = prepare_gate_4x4 (TwoQubitOp4x4 m TopBit)
-- GATE(8x8)[0][2] := SWAP(8x8)[1][2] * GATE(8x8)[0][1] * SWAP(8x8)[1][2]
prepare_gate_8x8 (TwoQubitOp8x8 m LBit BotBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 m LBit TopBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gate_swap MBit BotBit)
-- GATE(8x8)[1][0] := GATE(4x4)[1][0] `tensor` ID
prepare_gate_8x8 (TwoQubitOp8x8 m MBit TopBit) = gate_4x4 `tensor` gate_id
    where gate_4x4 = prepare_gate_4x4 (TwoQubitOp4x4 m BotBit)
-- GATE(8x8)[1][2] := ID `tensor` GATE(4x4)[0][1]
prepare_gate_8x8 (TwoQubitOp8x8 m MBit BotBit) = gate_id `tensor` gate_4x4
    where gate_4x4 = prepare_gate_4x4 (TwoQubitOp4x4 m TopBit)
-- GATE(8x8)[2][0] := SWAP(8x8)[0][2] * GATE(8x8)[0][2] * SWAP(8x8)[0][2]
prepare_gate_8x8 (TwoQubitOp8x8 m RBit TopBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 m LBit BotBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap LBit BotBit)
-- GATE(8x8)[2][1] := SWAP(8x8)[1][2] * GATE(8x8)[1][2] * SWAP(8x8)[1][2]
prepare_gate_8x8 (TwoQubitOp8x8 m RBit BotBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 m MBit BotBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap MBit BotBit)
-- GATE(8x8)[0][1][2] := GATE
prepare_gate_8x8 (ThreeQubitOp8x8 m LBit TopBit) = m
-- GATE(8x8)[0][2][1] := SWAP[1][2] * GATE(8x8)[0][1][2] * SWAP[1][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m LBit BotBit) = swap_8x8 * m * swap_8x8
    where swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap MBit BotBit)
-- GATE(8x8)[1][0][2] := SWAP[0][1] * GATE(8x8)[0][1][2] * SWAP[0][1]
prepare_gate_8x8 (ThreeQubitOp8x8 m MBit TopBit) = swap_8x8 * m * swap_8x8
    where swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap LBit TopBit)
-- GATE(8x8)[1][2][0] := SWAP[0][2] * GATE[1][0][2] * SWAP[0][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m MBit BotBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (ThreeQubitOp8x8 m MBit TopBit) 
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap LBit BotBit)
-- GATE(8x8)[2][0][1] := SWAP[1][2] * GATE[1][0][2] * SWAP[1][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m RBit TopBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (ThreeQubitOp8x8 m MBit TopBit) 
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap MBit BotBit)
-- GATE(8x8)[2][1][0] := SWAP[0][2] * GATE[0][1][2] * SWAP[0][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m RBit BotBit) = swap_8x8 * m * swap_8x8
    where swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 swap LBit BotBit)
