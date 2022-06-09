-- | Data types and functions to facilitate semantic evaluation of generator strings.

module DyadicRewrite.Generators.Semantics where

import qualified Data.Map

-----------------------------------------------------------------------------------------
-- * Types to describe semantic models.

-- | All semantic model currently supported.
data SemModel = MonoidalSem
              | DyadicOneSem
              | DyadicTwoSem
              | DyadicThreeSem
              deriving (Eq)

instance Show SemModel where
    show MonoidalSem    = "Monoidal"
    show DyadicOneSem   = "Dyadic(1)"
    show DyadicTwoSem   = "Dyadic(2)"
    show DyadicThreeSem = "Dyadic(3)"

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

-- | Returns the semantic value of a generator. If the generator does not exist, then
-- nothing is returned. To check if a generator is recorded, then use (hasGen dict id).
interpretGen :: GenDict a -> String -> Maybe a
interpretGen dict id = Data.Map.findWithDefault Nothing id dict
