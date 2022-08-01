-- | Data types and functions to facilitate semantic evaluation of generator strings.

module Lafont.Generators.Semantics where

import qualified Data.Map
import Data.Maybe
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

-----------------------------------------------------------------------------------------
-- * Semantic Comparison.

-- | Consumes a composition function, an identity element, a mapping of generators to
-- semantic values, and a word. If the word can be evaluated, then its semantic value is
-- returned. Otherwise (e.g., if a generator is missing a semantic value) then nothing is
-- returned.
semEval :: (Eq a) => (a -> a -> a) -> a -> GenDict a -> MonWord -> Maybe a
semEval _       id _    []         = Just id
semEval compose id gens (sym:word) =
    case (interpretGen gens $ name sym) of
        Just lhs -> case (semEval compose id gens word) of
            Just rhs -> Just (compose lhs rhs)
            Nothing  -> Nothing
        Nothing -> Nothing

-- | Consumes a composition function, an identity element, a mapping of generators to
-- semantic values, and two words. If both words can be evaluated, and their semantic
-- values are equal, then true is returned. If both words can be evaluated, and their
-- semenatic values are not equal, then false is returned. Otherwise, nothing is returned.
semComp :: (Eq a) => (a -> a -> a) -> a -> GenDict a -> MonWord -> MonWord -> Maybe Bool
semComp compose id gens lhs rhs =
    case (eval lhs) of
        Just lhs -> case (eval rhs) of
            Just rhs -> Just (lhs == rhs)
            Nothing -> Nothing
        Nothing -> Nothing
    where eval str = semEval compose id gens str
