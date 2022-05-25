-- | A collection of quantum gate primitives to describe quantum circuit semantics.

module DyadicRewrite.Generators.QubitGates where

import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Quantum.Synthesis.CliffordT

-----------------------------------------------------------------------------------------
-- * 

-- | Distinguishes a pair of qubits as top and bottom (as in a pair of horizontal wires).
data TwoBitPos = TopBit | BotBit

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

-- | The controlled X gate with control on qubit 1.
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

-- | A template for two qubit gates implementing a given operator. For a one qubit
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
