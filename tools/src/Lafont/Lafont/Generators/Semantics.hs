-- | Data types and functions to facilitate semantic evaluation of generator strings.

module Lafont.Generators.Semantics where

import qualified Data.Map                     as Map
import           Data.Maybe
import           Lafont.Common
import           Lafont.Generators.Categories

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
type GenDict a = Map.Map String (Maybe a)

-- | Creates an empty GenDict.
empty :: GenDict a
empty = Map.empty

-- | Returns true if a generator is already recorded.
hasGen :: GenDict a -> String -> Bool
hasGen dict id = Map.member id dict

-- | Records a identifier/semv pair inside a generator dictionary.
addGen :: GenDict a -> (String, Maybe a) -> GenDict a
addGen dict (id, semv) = Map.insert id semv dict

-- | Folds f over the (name, semv) entries of dict, and returns the accumulated value.
foldGens :: ((String, Maybe a) -> b -> b) -> b -> GenDict a -> b
foldGens f = Map.foldrWithKey adjust
    where adjust key semv acc = f (key, semv) acc

-- | Returns the alphabet described by the generators.
toAlphabet :: GenDict a -> [String]
toAlphabet = Map.keys

-- | Returns the semantic value of a generator. If the generator does not exist, then
-- nothing is returned. To check if a generator is recorded, then use (hasGen dict id).
interpretGen :: GenDict a -> String -> Maybe a
interpretGen dict id = Map.findWithDefault Nothing id dict

-----------------------------------------------------------------------------------------
-- * Semantic Comparison.

-- | Consumes a mapping of generators to monoidal semantic values and a word. If the word
-- can be evaluated, then its semantic value is returned. Otherwise (e.g., if a generator
-- is missing a semantic value) then nothing is returned.
semEval :: (MonoidObj a) => GenDict a -> MonWord -> Maybe a
semEval _    []         = Just identity
semEval gens (sym:word) =
    case interpretGen gens symName of
        Just lhs -> case semEval gens word of
            Just rhs -> compose lhs rhs
            Nothing  -> Nothing
        Nothing -> Nothing
    where symName = name sym

-- | Consumes a mapping of generators to monoidal semantic values and two words. If both
-- words can be evaluated, and their semantic values are equal, then true is returned. If
-- both words can be evaluated, and their semenatic values are not equal, then false is
-- returned. Otherwise, nothing is returned.
semComp :: (MonoidObj a) => GenDict a -> MonWord -> MonWord -> Maybe Bool
semComp gens lhs rhs =
    case semEval gens lhs of
        Just lhs -> case semEval gens rhs of
            Just rhs -> equate lhs rhs
            Nothing  -> Nothing
        Nothing -> Nothing
