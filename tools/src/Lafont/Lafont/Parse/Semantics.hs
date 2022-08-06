-- | Realizations of SemParser.

module Lafont.Parse.Semantics where

import           Data.Maybe
import           Lafont.Common
import           Lafont.Generators.QubitGates
import           Lafont.Parse.Common
import           Lafont.Parse.MonWords
import qualified Quantum.Synthesis.Matrix     as QMat
import qualified Quantum.Synthesis.Ring       as QRing

-----------------------------------------------------------------------------------------
-- * General Semantic Parsing.

-- | A function used to parse a value given a semantic model. Takes as input a textual
-- representation of the semantic value. Returns either a parsing error (as a string) or
-- a semantic model value.
type SemParser a = (String -> Either String a)

-----------------------------------------------------------------------------------------
-- * Monoidal Semantics.

-- | Always returns an error message since free monoids do not have semantics beyond
-- their generators and relations.
parseMonoidalSem :: SemParser ()
parseMonoidalSem _ = Left "Monoidal generators do not support semantics"

-----------------------------------------------------------------------------------------
-- * Quantum Operator Semantics (General Interpretation Framework).

-- | Either contains an error as to why a user-provided gate is invalid, or contains the
-- gate described by the user.
type QGateSemRes n a = Either String (Unitary n a)

-- | Describes a subgroup of U(n). Provides the nxn identity matrix, functions to
-- interpret symbols (with up to 3 arguments) as generators in U(n). Each interpretation
-- function is optional.
data QGateSem n a = QGateSem (Unitary n a)
                             (Maybe (String -> QGateSemRes n a))
                             (Maybe (String -> Int -> QGateSemRes n a))
                             (Maybe (String -> Int -> Int -> QGateSemRes n a))
                             (Maybe (String -> Int -> Int -> Int -> QGateSemRes n a))

-- | Consumes interpretation semantics (for up to 3 arguments) and a symbol. If the
-- symbol is a valid gate with respect to the semantics, then that gate is returned.
-- Otherwise, an error message is returned.
interpretNQubitSymbol :: QGateSem n a -> Symbol -> QGateSemRes n a
interpretNQubitSymbol (QGateSem _ sem0 sem1 sem2 sem3) sym =
    case args sym of
        []        -> maybe (Left err) (\f -> f op) sem0
        [a]       -> maybe (Left err) (\f -> f op a) sem1
        [a, b]    -> maybe (Left err) (\f -> f op a b) sem2
        [a, b, c] -> maybe (Left err) (\f -> f op a b c) sem3
        _         -> Left err
    where op  = name sym
          err = "Invalid argument count in: " ++ display sym

-- | Consumes interpretation semantics (for up to 3 arguments) and a word. If the word is
-- a valid gate with respect to the semantics, then that gate is returned. Otherwise, an
-- error message is returned.
interpretNQubitWord :: (QMat.Nat n, Num a) => QGateSem n a -> MonWord -> QGateSemRes n a
interpretNQubitWord (QGateSem id _ _ _ _ ) [] = Right id
interpretNQubitWord sem (sym:word)            =
    case interpretNQubitWord sem word of
        Left  msg  -> Left msg
        Right rhs -> case interpretNQubitSymbol sem sym of
            Left err  -> Left err
            Right lhs -> Right (lhs * rhs)

-- | Consumes interpretation semantics (for up to 3 arguments) and a string. If the
-- string can be interpreted as a word that is a valid gate with respect to the
-- semantics, then that gate is returned. Otherwise, an error message is returned.
interpretNQubitGate :: (QMat.Nat n, Num a) => QGateSem n a -> SemParser (Unitary n a)
interpretNQubitGate sem str =
    case parseMonWord trimmed of
        Just (word, eol) -> if snd (trimSpacing eol) == ""
                            then interpretNQubitWord sem word
                            else Left "Expected a single word"
        Nothing -> Left "Expected a word"
    where (_, trimmed) = trimSpacing $ stripComments str

-----------------------------------------------------------------------------------------
-- * Quantum Operator Semantics: Clifford(D) on Two Qubits.

type TwoQubitDyadic = Unitary QMat.Four QRing.Dyadic

interpretUnique2QubitDOp4x4 :: String -> QGateSemRes QMat.Four QRing.Dyadic
interpretUnique2QubitDOp4x4 "CZ"   = Right gateCZ
interpretUnique2QubitDOp4x4 "K"    = Right gateK
interpretUnique2QubitDOp4x4 "SWAP" = Right gateSwap
interpretUnique2QubitDOp4x4 op     = Left ("Unknown two qubit operator: " ++ op)

make1QubitDOp4x4 :: String -> TwoBitPos -> QGateSemRes QMat.Four QRing.Dyadic
make1QubitDOp4x4 "X" a = Right (prepare_gate_4x4 (OneQubitOp4x4 gateX a))
make1QubitDOp4x4 "Z" a = Right (prepare_gate_4x4 (OneQubitOp4x4 gateZ a))
make1QubitDOp4x4 op  _ = Left ("Unknown one qubit operator: " ++ op)

interpret1QubitDOp4x4 :: String -> Int -> QGateSemRes QMat.Four QRing.Dyadic
interpret1QubitDOp4x4 op 0 = make1QubitDOp4x4 op TopBit
interpret1QubitDOp4x4 op 1 = make1QubitDOp4x4 op BotBit
interpret1QubitDOp4x4 op a = Left ("Invalid gate position: " ++ op ++ posStr)
    where posStr = "[" ++ show a ++ "]"

make2QubitDOp4x4 :: String -> TwoBitPos -> QGateSemRes QMat.Four QRing.Dyadic
make2QubitDOp4x4 "CX" a = Right (prepare_gate_4x4 (TwoQubitOp4x4 gateCX a))
make2QubitDOp4x4 op   _ = Left ("Unknown two qubit operator: " ++ op)

interpret2QubitDOp4x4 :: String -> Int -> Int -> QGateSemRes QMat.Four QRing.Dyadic
interpret2QubitDOp4x4 op 0 1 = make2QubitDOp4x4 op TopBit
interpret2QubitDOp4x4 op 1 0 = make2QubitDOp4x4 op BotBit
interpret2QubitDOp4x4 op a b = Left ("Invalid gate position: " ++ op ++ posStr)
    where posStr = "[" ++ show a ++ "][" ++ show b ++ "]"

sem2QubitDOp :: QGateSem QMat.Four QRing.Dyadic
sem2QubitDOp = QGateSem (gateId `QMat.tensor` gateId)
                        (Just interpretUnique2QubitDOp4x4)
                        (Just interpret1QubitDOp4x4)
                        (Just interpret2QubitDOp4x4)
                        Nothing

interpret2QubitCliffordDTofGate :: SemParser TwoQubitDyadic
interpret2QubitCliffordDTofGate = interpretNQubitGate sem2QubitDOp

-----------------------------------------------------------------------------------------
-- * Quantum Operator Semantics: Clifford(D)+Tof on Three Qubits.

type ThreeQubitDyadic = Unitary QMat.Eight QRing.Dyadic

make1QubitDOp8x8 :: String -> ThreeBitPos -> QGateSemRes QMat.Eight QRing.Dyadic
make1QubitDOp8x8 "X" a = Right (prepare_gate_8x8 (OneQubitOp8x8 gateX a))
make1QubitDOp8x8 "Z" a = Right (prepare_gate_8x8 (OneQubitOp8x8 gateZ a))
make1QubitDOp8x8 op  _ = Left ("Unknown one qubit operator: " ++ op)

interpret1QubitDOp8x8 :: String -> Int -> QGateSemRes QMat.Eight QRing.Dyadic
interpret1QubitDOp8x8 op 0 = make1QubitDOp8x8 op LBit
interpret1QubitDOp8x8 op 1 = make1QubitDOp8x8 op MBit
interpret1QubitDOp8x8 op 2 = make1QubitDOp8x8 op RBit
interpret1QubitDOp8x8 op a = Left ("Invalid gate position: " ++ op ++ posStr)
    where posStr = "[" ++ show a ++ "]"

make2QubitDOp8x8 :: String -> ThreeBitPos -> TwoBitPos -> QGateSemRes QMat.Eight QRing.Dyadic
make2QubitDOp8x8 "CX"   a b = Right (prepare_gate_8x8 (TwoQubitOp8x8 gateCX a b))
make2QubitDOp8x8 "CZ"   a b = Right (prepare_gate_8x8 (TwoQubitOp8x8 gateCZ a b))
make2QubitDOp8x8 "K"    a b = Right (prepare_gate_8x8 (TwoQubitOp8x8 gateK a b))
make2QubitDOp8x8 "SWAP" a b = Right (prepare_gate_8x8 (TwoQubitOp8x8 gateSwap a b))
make2QubitDOp8x8 op     _ _ = Left ("Unknown two qubit operator: " ++ op)

interpret2QubitDOp8x8 :: String -> Int -> Int -> QGateSemRes QMat.Eight QRing.Dyadic
interpret2QubitDOp8x8 op 0 1 = make2QubitDOp8x8 op LBit TopBit
interpret2QubitDOp8x8 op 0 2 = make2QubitDOp8x8 op LBit BotBit
interpret2QubitDOp8x8 op 1 0 = make2QubitDOp8x8 op MBit TopBit
interpret2QubitDOp8x8 op 1 2 = make2QubitDOp8x8 op MBit BotBit
interpret2QubitDOp8x8 op 2 0 = make2QubitDOp8x8 op RBit TopBit
interpret2QubitDOp8x8 op 2 1 = make2QubitDOp8x8 op RBit BotBit
interpret2QubitDOp8x8 op a b = Left ("Invalid gate position: " ++ op ++ posStr)
    where posStr = "[" ++ show a ++ "][" ++ show b ++ "]"

make3QubitDOp8x8 :: String -> ThreeBitPos -> TwoBitPos -> QGateSemRes QMat.Eight QRing.Dyadic
make3QubitDOp8x8 "CCX" a b = Right (prepare_gate_8x8 (ThreeQubitOp8x8 gateTof a b))
make3QubitDOp8x8 op    _ _ = Left ("Unknown three qubit operator: " ++ op)

interpret3QubitDOp8x8 :: String -> Int -> Int ->Int -> QGateSemRes QMat.Eight QRing.Dyadic
interpret3QubitDOp8x8 op 0 1 2 = make3QubitDOp8x8 op LBit TopBit
interpret3QubitDOp8x8 op 0 2 1 = make3QubitDOp8x8 op LBit BotBit
interpret3QubitDOp8x8 op 1 0 2 = make3QubitDOp8x8 op MBit TopBit
interpret3QubitDOp8x8 op 1 2 0 = make3QubitDOp8x8 op MBit BotBit
interpret3QubitDOp8x8 op 2 0 1 = make3QubitDOp8x8 op RBit TopBit
interpret3QubitDOp8x8 op 2 1 0 = make3QubitDOp8x8 op RBit BotBit
interpret3QubitDOp8x8 op a b c = Left ("Invalid gate position: " ++ op ++ posStr)
    where posStr = "[" ++ show a ++ "][" ++ show b ++ "][" ++ show c ++ "]"

sem3QubitDOp :: QGateSem QMat.Eight QRing.Dyadic
sem3QubitDOp = QGateSem (gateId `QMat.tensor` gateId `QMat.tensor` gateId)
                        Nothing
                        (Just interpret1QubitDOp8x8)
                        (Just interpret2QubitDOp8x8)
                        (Just interpret3QubitDOp8x8)

interpret3QubitCliffordDTofGate :: SemParser ThreeQubitDyadic
interpret3QubitCliffordDTofGate = interpretNQubitGate sem3QubitDOp
