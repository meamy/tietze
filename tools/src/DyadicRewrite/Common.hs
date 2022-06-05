module DyadicRewrite.Common where

-----------------------------------------------------------------------------------------
-- * Circuits.

-- | Specifies the name (id) of an operator. If the operator is parameterized, then the
-- parameters are also given.
data Gate = Gate { name :: String
                 , params :: [Int]
                 } deriving (Eq)

instance Show Gate where
    show (Gate name params) = foldl (\str n -> str ++ "[" ++ (show n) ++ "]") name params

-- | Type definition for strings of gates.
type Circuit = [Gate]
