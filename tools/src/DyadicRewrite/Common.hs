module DyadicRewrite.Common where

-----------------------------------------------------------------------------------------
-- * Circuits.

-- | Specifies the name (id) of an operator. If the operator is parameterized, then the
-- parameters are also given.
data Gate = Gate { id :: String
                 , params :: [Int]
                 } deriving (Show,Eq)

type Circuit = [Gate]
