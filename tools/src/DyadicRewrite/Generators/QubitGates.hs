-- | A collection of quantum gate primitives to describe quantum circuit semantics.

module DyadicRewrite.Generators.QubitGates where

import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Quantum.Synthesis.CliffordT

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
