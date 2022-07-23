-- | Data types and functions to facilitate semantic evaluation of generator strings.

module Lafont.Generators.Semantics where

import qualified Data.Map
import Lafont.Common

-----------------------------------------------------------------------------------------
-- * Semantic Model Descriptions.

-- | All semantic model currently supported.
data SemModel = MonoidalSem
              | DyadicTwoSem
              | DyadicThreeSem
              deriving (Eq,Show)

instance Display SemModel where
    display MonoidalSem    = "Monoidal"
    display DyadicTwoSem   = "Dyadic(2)"
    display DyadicThreeSem = "Dyadic(3)"

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

-- | Folds f over the (name, semv) entries of dict, and returns the accumulated value.
foldGens :: ((String, Maybe a) -> b -> b) -> b -> GenDict a -> b
foldGens f init dict = Data.Map.foldrWithKey fadj init dict
    where fadj key semv acc = f (key, semv) acc

-- | Returns the alphabet described by the generators.
toAlphabet :: GenDict a -> [String]
toAlphabet dict = Data.Map.keys dict

-- | Returns the semantic value of a generator. If the generator does not exist, then
-- nothing is returned. To check if a generator is recorded, then use (hasGen dict id).
interpretGen :: GenDict a -> String -> Maybe a
interpretGen dict id = Data.Map.findWithDefault Nothing id dict
