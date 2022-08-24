-- | General-purpose data types and functions.

module Lafont.Common (
    Display ( .. ),
    Symbol ( .. ),
    MonWord
) where

-----------------------------------------------------------------------------------------
-- * Common Classes

-- | Describes a type that admits a user-friendly ASCII format.
class Display a where
    display :: a -> String

-----------------------------------------------------------------------------------------
-- * Common Word Types.

-- | Specifies a symbol which represents a generator. Built-in generators may allow for
-- parameterization (e.g., the k-out-of-n qubits that a k-qubit gate is applied to in an
-- n-qubit circuit). In this case, the parameters are given alongside the generator name
-- as arguments.
data Symbol = Symbol { name :: String
                     , args :: [Int]
                     } deriving (Eq,Show)

instance Display Symbol where
    display (Symbol sym args) = foldl fn sym args
        where fn str n = str ++ "[" ++ show n ++ "]"

-- | Type definition for a string of generators (i.e., a word in a free monoid).
type MonWord = [Symbol]
