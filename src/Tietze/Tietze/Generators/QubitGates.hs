-- | A collection of quantum gate primitives to describe quantum circuit semantics.

module Tietze.Generators.QubitGates
  ( TwoBitPos (..)
  , ThreeBitPos (..)
  , TwoQubitGate (..)
  , ThreeQubitGate (..)
  , Unitary
  , gateId
  , gateX
  , gateY
  , gateZ
  , gateH
  , gateSwap
  , gateCX
  , gateCZ
  , gateK
  , gateTof
  , gateCCZ
  , prepare_gate_4x4
  , prepare_gate_8x8
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Quantum.Synthesis.CliffordT as QCliffT
import qualified Quantum.Synthesis.Matrix as QMat
import qualified Quantum.Synthesis.Ring as QRing

-----------------------------------------------------------------------------------------
-- * Helper Types.

type Unitary n a = QMat.Matrix n n a

-----------------------------------------------------------------------------------------
-- * Template Specification Types.

-- | Distinguishes a pair of qubits as top and bottom (as in a pair of horizontal wires).
data TwoBitPos = TopBit | BotBit

-- | Distinguishes a tiple of qubits as left, middle, and right (as in a tiple of
-- vertical wires).
data ThreeBitPos = LBit | MBit | RBit

-----------------------------------------------------------------------------------------
-- * One-Qubit Gates.

-- | The 2x2 identity matrix.
gateId :: (Num a) => Unitary QMat.Two a
gateId = QMat.matrix2x2 (1, 0)
                        (0, 1)

-- | The Pauli X gate.
gateX :: (Num a) => Unitary QMat.Two a
gateX = QCliffT.u2_X

-- | The Pauli Y gate.
gateY :: (QRing.ComplexRing a) => Unitary QMat.Two a
gateY = QCliffT.u2_Y

-- | The Pauli Z gate.
gateZ :: (Num a) => Unitary QMat.Two a
gateZ = QCliffT.u2_Z

-- | The Hadamard gate.
gateH :: (QRing.RootHalfRing a) => Unitary QMat.Two a
gateH = QCliffT.u2_H

-----------------------------------------------------------------------------------------
-- * Two-Qubit Gate Templates.

-- | The (unique) two qubit swap gate.
gateSwap :: (Num a) => Unitary QMat.Four a
gateSwap = QMat.swap

-- | The controlled X gate with control on qubit 0.
gateCX :: (Num a) => Unitary QMat.Four a
gateCX = QMat.cnot

-- | The controlled Z gate.
gateCZ :: (Num a) => Unitary QMat.Four a
gateCZ = QMat.matrix4x4 (1,0,0, 0)
                        (0,1,0, 0)
                        (0,0,1, 0)
                        (0,0,0,-1)

-- | The tensor of two Hadamard gates.
gateK :: (QRing.HalfRing a) => Unitary QMat.Four a
gateK = QRing.half `QMat.scalarmult` QMat.matrix4x4 (1, 1, 1, 1)
                                                    (1,-1, 1,-1)
                                                    (1, 1,-1,-1)
                                                    (1,-1,-1, 1)

-- | A template for two qubit gates that implement given operators. For a one qubit
-- operation, the position specifies which qubit the gate is applied to. For a two qubit
-- gate, the position specifies which qubit is the primary input (e.g., a control).
data TwoQubitGate a = OneQubitOp4x4 (Unitary QMat.Two a) TwoBitPos
                    | TwoQubitOp4x4 (Unitary QMat.Four a) TwoBitPos

-- | Computes the 4x4 matrice of the two qubit gate described by a template.
prepare_gate_4x4 :: (QRing.Ring a) => TwoQubitGate a -> Unitary QMat.Four a
prepare_gate_4x4 (OneQubitOp4x4 m TopBit) = m `QMat.tensor` gateId
prepare_gate_4x4 (OneQubitOp4x4 m BotBit) = gateId `QMat.tensor` m
prepare_gate_4x4 (TwoQubitOp4x4 m TopBit) = m
prepare_gate_4x4 (TwoQubitOp4x4 m BotBit) = gateSwap * m * gateSwap

-----------------------------------------------------------------------------------------
-- * Three-Qubit Gate Templates.

-- | The doubly controlled X gate with controls on qubits 0 and 1. Otherwise known as a
-- Toffoli gate.
gateTof :: (Num a) => Unitary QMat.Eight a
gateTof = QMat.matrix_of_rows [[1,0,0,0,0,0,0,0],
                               [0,1,0,0,0,0,0,0],
                               [0,0,1,0,0,0,0,0],
                               [0,0,0,1,0,0,0,0],
                               [0,0,0,0,1,0,0,0],
                               [0,0,0,0,0,1,0,0],
                               [0,0,0,0,0,0,0,1],
                               [0,0,0,0,0,0,1,0]]

-- |
gateCCZ :: (Num a) => Unitary QMat.Eight a
gateCCZ = QMat.matrix_of_rows [[1,0,0,0,0,0,0, 0],
                               [0,1,0,0,0,0,0, 0],
                               [0,0,1,0,0,0,0, 0],
                               [0,0,0,1,0,0,0, 0],
                               [0,0,0,0,1,0,0, 0],
                               [0,0,0,0,0,1,0, 0],
                               [0,0,0,0,0,0,1, 0],
                               [0,0,0,0,0,0,0,-1]]

-- | A template for three qubit gates that implement given operators. For a one qubit
-- operation, the position specifies which qubit the gate is applied to. For two and
-- three qubit gates, the positions specify the primary and secondary input. The primary
-- input (e.g. a control) is any of the three inputs. The secondary input (e.g., the
-- bit controlled by a two qubit operation, or the second control in a three qubit
-- operation) is any of the two remaining inputs.
data ThreeQubitGate a = OneQubitOp8x8 (Unitary QMat.Two a) ThreeBitPos
                      | TwoQubitOp8x8 (Unitary QMat.Four a) ThreeBitPos TwoBitPos
                      | ThreeQubitOp8x8 (Unitary QMat.Eight a) ThreeBitPos TwoBitPos

-- | Computes the 8x8 matrice of the three qubit gate described by a template.
prepare_gate_8x8 :: (QRing.Ring a) => ThreeQubitGate a -> Unitary QMat.Eight a
-- GATE(8x8)[0] := GATE(4x4)[0] `tensor` ID
prepare_gate_8x8 (OneQubitOp8x8 m LBit) = gate_4x4 `QMat.tensor` gateId
    where gate_4x4 = prepare_gate_4x4 (OneQubitOp4x4 m TopBit)
-- GATE(8x8)[1] := GATE(4x4)[1] `tensor` ID
prepare_gate_8x8 (OneQubitOp8x8 m MBit) = gate_4x4 `QMat.tensor` gateId
    where gate_4x4 = prepare_gate_4x4 (OneQubitOp4x4 m BotBit)
-- GATE(8x8)[2] := ID `tensor` GATE(4x4)[1]
prepare_gate_8x8 (OneQubitOp8x8 m RBit) = gateId `QMat.tensor` gate_4x4
    where gate_4x4 = prepare_gate_4x4 (OneQubitOp4x4 m BotBit)
-- GATE(8x8)[0][1] := GATE(4x4)[0][1] `tensor` ID
prepare_gate_8x8 (TwoQubitOp8x8 m LBit TopBit) = gate_4x4 `QMat.tensor` gateId
    where gate_4x4 = prepare_gate_4x4 (TwoQubitOp4x4 m TopBit)
-- GATE(8x8)[0][2] := SWAP(8x8)[1][2] * GATE(8x8)[0][1] * SWAP(8x8)[1][2]
prepare_gate_8x8 (TwoQubitOp8x8 m LBit BotBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 m LBit TopBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap MBit BotBit)
-- GATE(8x8)[1][0] := GATE(4x4)[1][0] `tensor` ID
prepare_gate_8x8 (TwoQubitOp8x8 m MBit TopBit) = gate_4x4 `QMat.tensor` gateId
    where gate_4x4 = prepare_gate_4x4 (TwoQubitOp4x4 m BotBit)
-- GATE(8x8)[1][2] := ID `tensor` GATE(4x4)[0][1]
prepare_gate_8x8 (TwoQubitOp8x8 m MBit BotBit) = gateId `QMat.tensor` gate_4x4
    where gate_4x4 = prepare_gate_4x4 (TwoQubitOp4x4 m TopBit)
-- GATE(8x8)[2][0] := SWAP(8x8)[0][2] * GATE(8x8)[0][2] * SWAP(8x8)[0][2]
prepare_gate_8x8 (TwoQubitOp8x8 m RBit TopBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 m LBit BotBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap LBit BotBit)
-- GATE(8x8)[2][1] := SWAP(8x8)[1][2] * GATE(8x8)[1][2] * SWAP(8x8)[1][2]
prepare_gate_8x8 (TwoQubitOp8x8 m RBit BotBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 m MBit BotBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap MBit BotBit)
-- GATE(8x8)[0][1][2] := GATE
prepare_gate_8x8 (ThreeQubitOp8x8 m LBit TopBit) = m
-- GATE(8x8)[0][2][1] := SWAP[1][2] * GATE(8x8)[0][1][2] * SWAP[1][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m LBit BotBit) = swap_8x8 * m * swap_8x8
    where swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap MBit BotBit)
-- GATE(8x8)[1][0][2] := SWAP[0][1] * GATE(8x8)[0][1][2] * SWAP[0][1]
prepare_gate_8x8 (ThreeQubitOp8x8 m MBit TopBit) = swap_8x8 * m * swap_8x8
    where swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap LBit TopBit)
-- GATE(8x8)[1][2][0] := SWAP[0][2] * GATE[1][0][2] * SWAP[0][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m MBit BotBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (ThreeQubitOp8x8 m MBit TopBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap LBit BotBit)
-- GATE(8x8)[2][0][1] := SWAP[1][2] * GATE[1][0][2] * SWAP[1][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m RBit TopBit) = swap_8x8 * gate_8x8 * swap_8x8
    where gate_8x8 = prepare_gate_8x8 (ThreeQubitOp8x8 m MBit TopBit)
          swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap MBit BotBit)
-- GATE(8x8)[2][1][0] := SWAP[0][2] * GATE[0][1][2] * SWAP[0][2]
prepare_gate_8x8 (ThreeQubitOp8x8 m RBit BotBit) = swap_8x8 * m * swap_8x8
    where swap_8x8 = prepare_gate_8x8 (TwoQubitOp8x8 gateSwap LBit BotBit)
