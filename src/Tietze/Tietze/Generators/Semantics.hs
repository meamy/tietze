-- | Data types and functions to facilitate semantic evaluation of generator strings.

module Tietze.Generators.Semantics
  ( SemModel (..)
  , GenDict
  , display
  , semToTok
  , empty
  , hasGen
  , addGen
  , foldGens
  , toAlphabet
  , interpretGen
  , semEval
  , semComp
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map
import Tietze.Common
  ( Display (..)
  , MonWord
  , Symbol (..)
  )
import Tietze.Generators.Categories (MonoidObj (..))
import Tietze.Maybe (branchJust)
import Tietze.String (displayList)

-----------------------------------------------------------------------------------------
-- * Semantic Model Descriptions.

-- | All semantic model currently supported.
data SemModel = MonoidSem
              | DyadicTwoSem
              | DyadicThreeSem
              | MultModPSem [Int]
              | AddModPSem [Int]
              deriving (Eq,Show)

-- | Returns the token used to indicate a semantic model. For example, if the semantic
-- model is taken to be MonoidSem, then both the semantic token and the semantic line are
-- Monoid. On the other hand, if the semantic model is taken to be AddModP, then the
-- semantic token is AddModP, whereas the semantic line might be AddModP(0, 0, 5, 7, 3).
semToTok :: SemModel -> String
semToTok MonoidSem       = "Monoid"
semToTok DyadicTwoSem    = "Dyadic(2)"
semToTok DyadicThreeSem  = "Dyadic(3)"
semToTok (MultModPSem _) = "MultModP"
semToTok (AddModPSem _)  = "AddModP"

-- | Returns the arguments passed to a semantic model. For example, Monoid requires no
-- parameters. On the other hand, the additive group of integers moudlo p would be
-- parameterized by the value p.
semToArgs :: SemModel -> String
semToArgs (MultModPSem pvals) = "(" ++ displayList pvals ++ ")"
semToArgs (AddModPSem pvals)  = "(" ++ displayList pvals ++ ")"
semToArgs _                   = ""

instance Display SemModel where
    display sem = semToTok sem ++ semToArgs sem

-----------------------------------------------------------------------------------------
-- * Generator Dictionary.

-- | A mapping from generator symbols (strings) to their semantic values of type a.
newtype GenDict a = GenDict (Map.Map String (Maybe a)) deriving (Eq,Show)

-- | Creates an empty GenDict.
empty :: GenDict a
empty = GenDict Map.empty

-- | Returns true if a generator is already recorded.
hasGen :: GenDict a -> String -> Bool
hasGen (GenDict dict) id = Map.member id dict

-- | Records a identifier/semv pair inside a generator dictionary.
addGen :: GenDict a -> (String, Maybe a) -> GenDict a
addGen (GenDict dict) (id, semv) = GenDict (Map.insert id semv dict)

-- | Folds f over the (name, semv) entries of dict, and returns the accumulated value.
foldGens :: ((String, Maybe a) -> b -> b) -> b -> GenDict a -> b
foldGens f init (GenDict dict) = Map.foldrWithKey adjust init dict
    where adjust key semv = f (key, semv)

-- | Returns the alphabet described by the generators.
toAlphabet :: GenDict a -> [String]
toAlphabet (GenDict dict) = Map.keys dict

-- | Returns the semantic value of a generator. If the generator does not exist, then
-- nothing is returned. To check if a generator is recorded, then use (hasGen dict id).
interpretGen :: GenDict a -> String -> Maybe a
interpretGen (GenDict dict) id = Map.findWithDefault Nothing id dict

-----------------------------------------------------------------------------------------
-- * Semantic Comparison.

-- | Consumes a mapping of generators to monoid semantic values and a word. If the word
-- can be evaluated, then its semantic value is returned. Otherwise (e.g., if a generator
-- is missing a semantic value) then nothing is returned.
semEval :: (MonoidObj a) => GenDict a -> MonWord -> Maybe a
semEval _    []      = Just identity
semEval gens (sym:w) = branchJust (interpretGen gens $ name sym)
                                  (branchJust (semEval gens w) . compose)

-- | Consumes a mapping of generators to monoidal semantic values and two words. If both
-- words can be evaluated, and their semantic values are equal, then true is returned. If
-- both words can be evaluated, and their semenatic values are not equal, then false is
-- returned. Otherwise, nothing is returned.
semComp :: (MonoidObj a) => GenDict a -> MonWord -> MonWord -> Maybe Bool
semComp gens lhs rhs = branchJust (semEval gens lhs)
                                  (branchJust (semEval gens rhs) . equate)
