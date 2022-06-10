module DyadicRewrite.Common where

-----------------------------------------------------------------------------------------
-- * Common Word Types.

-- | Specifies a symbol which represents a generator. Built-in generators may allow for
-- parameterization (e.g., the k-out-of-n qubits that a k-qubit gate is applied to in an
-- n-qubit circuit). In this case, the parameters are given alongside the generator name
-- as arguments.
data Symbol = Symbol { name :: String
                     , args :: [Int]
                     } deriving (Eq)

instance Show Symbol where
    show (Symbol name args) = foldl (\str n -> str ++ "[" ++ (show n) ++ "]") name args

-- | Type definition for a string of generators (i.e., a word in a free monoid).
type MonWord = [Symbol]
