-- | Parsing functions for circuits.

module DyadicRewrite.Parse.Circuits where

import DyadicRewrite.Common
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Utilities to parse circuit parameters..

-- | Helper function to parse: [ <NAT> ].
parseParam :: String -> Maybe (Int, String)
parseParam []        = Nothing
parseParam ('[':str) =
    case (parseNat str) of
        Just (n, post) -> case (parseFromSeps ["]"] post) of
            Just (_, post') -> Just (n, post')
            Nothing         -> Nothing
        Nothing -> Nothing
parseParam _ = Nothing

-- | Consumes a string (str). If there exists a string pre = [i1][i2][i3]...[in] such
-- that i1, i2, i3, ..., in are natural numbers, str = pre + post, and pre is the maximal
-- such prefix, then ([i1,i2,i3,...,in], post) is returned. Otherwise, nothing is
-- returned.
parseParams :: String -> ([Int], String)
parseParams str =
    case (parseParam str) of
        Just (param, post) -> let (params, post') = (parseParams post)
                              in (param:params, post')
        Nothing -> ([], str)

-----------------------------------------------------------------------------------------
-- * Utilities to parse circuit gates.

-- | Consumes a string (str). If there exists a string pre of the form <ID><PARAMS> that
-- str = pre + post, then returns (Gate <ID> <PARAM>, post) where pre is the maximal such
-- prefix. Otherwise, nothing is returned.
parseGate :: String -> Maybe (Gate, String)
parseGate str = 
    case (parseId str) of
        Just (id, post) -> let (params, post') = (parseParams post)
                           in Just ((Gate id params), post')
        Nothing -> Nothing

-----------------------------------------------------------------------------------------
-- * Circuit parsing functions.

-- | Data type used to distinguish separators in circuit string.
data CircuitSep = CircuitDot | CircuitEnd

-- | Helper method to classify the next separator in a circuit. If the separator is a
-- part of the circuit, then the characters are consumed. If there is no match, then
-- nothing is returned.
parseCircuitSep :: String -> Maybe (CircuitSep, String)
parseCircuitSep ""         = Just (CircuitEnd, "")
parseCircuitSep (' ':post) = Just (CircuitEnd, ' ':post)
parseCircuitSep ('.':post) = Just (CircuitDot, post)
parseCircuitSep _          = Nothing

-- | Helper method to append a known gate to the front of a circuit parsed by a circuit
-- parser. If the circuit string does not parse, then nothing is returned.
--
-- Mutually Depends On: parseCircuit
joinAndParseCircuit :: Gate -> String -> Maybe (Circuit, String)
joinAndParseCircuit gate str =
    case (parseCircuit str) of
        Just (circ, post) -> Just (gate:circ, post)
        Nothing           -> Nothing

-- | Consumes a string (str). If there exists a circuit string pre = G1.G2.G3...Gn such
-- that G1, G2, G3, ..., Gn are gates, str = pre + post, and pre is the maximal such
-- prefix of str, then ([G1, G2, G3, ..., Gn], post) is returned. Otherwise, nothing is
-- returned.
--
-- Mutually Depends On: joinAndParseCircuit
parseCircuit :: String -> Maybe (Circuit, String)
parseCircuit str =
    case (parseGate str) of
        Just (gate, post) -> case (parseCircuitSep post) of
            Just (CircuitEnd, post') -> Just (gate:[], post')
            Just (CircuitDot, post') -> (joinAndParseCircuit gate post')
            Nothing                  -> Nothing
        Nothing -> Nothing
