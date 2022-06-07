-- | Data types and functions to facilitate semantic evaluation of generator strings.

module DyadicRewrite.Generators.Semantics where

import qualified Data.Map

-----------------------------------------------------------------------------------------
-- * Generator Dictionary.

-- | A mapping from generator symbols (strings) to their semantic values of type a.
type GenDict a = Data.Map.Map String (Maybe a)

-- | Creates an empty GenDict.
empty :: GenDict a
empty = Data.Map.empty

-- | Returns true if a generator is already recorded.
hasGen :: GenDict a -> String -> Bool
hasGen dict id = Data.Map.member id dict

-- | Records a identifier/semv pair inside a generator dictionary.
addGen :: GenDict a -> (String, Maybe a) -> GenDict a
addGen dict (id, semv) = Data.Map.insert id semv dict

-- | Returns the semantic value of a generator. If the generator does not exist, then
-- nothing is returned. To check if a generator is recorded, then use (hasGen dict id).
interpretGen :: GenDict a -> String -> Maybe a
interpretGen dict id = Data.Map.findWithDefault Nothing id dict
