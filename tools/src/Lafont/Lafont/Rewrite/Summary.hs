{- HLINT ignore RewritePreamble "Use newtype instead of data" -}

-- | This module provides data-types to summarize derivations, and functions to convert
-- summarizations into rules.

module Lafont.Rewrite.Summary where

import           Data.Map              as Map
import           Data.Set              as Set
import           Lafont.Common
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Abstract Summaries of a Derivation.

-- | Maintains metadata about a derivation.
data RewritePreamble = RewritePreamble { propName :: Maybe String
                                       } deriving (Show,Eq)

-- | A description of a derivation that abstracts away all proof details (rewrites).
data DerivationSummary = DerivationSummary { meta    :: RewritePreamble
                                           , initial :: MonWord
                                           , final   :: MonWord
                                           } deriving (Eq,Show)

-----------------------------------------------------------------------------------------
-- * Functions to Abstract Derivation Summaries as Rules.

-- | A mapping from derivation names to their equatioanl flags. If the equational flag of
-- a derived relation is true, then the derived relation may be applied in any direction.
type EqMap = Map.Map String Bool

-- | Reads an entry from an EqMap.
isEquationalDerivation :: EqMap -> String -> Maybe Bool
isEquationalDerivation map name = name `Map.lookup` map

-- | Consumes an equationality map (emap) a summary recorded in the map (sum). Returns
-- true if and only if emap contains a true entry for sum. Note that when sum is unnamed,
-- it is trivially true that emap does not contain an entry for sum.
isSummaryEquational :: EqMap -> DerivationSummary -> Bool
isSummaryEquational emap sum =
    case propName $ meta sum of
        Nothing   -> False
        Just name -> case isEquationalDerivation emap name of
            Just True -> True
            _         -> False

-- | Consumes an equationality map (emap) a summary recorded in the map (sum). Returns a
-- new derived rule which meets the specifications of sum. Requires that sum is named. If
-- sum is not recorded in emap, then sum is assumed to be non-equational.
createSummaryRule :: EqMap -> DerivationSummary -> RewriteRule
createSummaryRule emap sum = RewriteRule lhs rhs eqn from
    where lhs  = initial sum
          rhs  = final sum
          eqn  = isSummaryEquational emap sum
          from = propName (meta sum)

-----------------------------------------------------------------------------------------
-- * Functions to Register Derivations as Relations.

-- | A collection of unique derived relation symbols.
type DRuleSet = Set.Set String

-- | An empty collection of derived relation symbols.
nullRuleSet :: DRuleSet
nullRuleSet = Set.empty

-- | Returns true if a DRuleSet contains a relation symbol.
hasDerivedRule :: DRuleSet -> String -> Bool
hasDerivedRule rels rel = rel `Set.member` rels

-- | Adds a relation symbol to a DRuleSet.
addDerivedRule :: DRuleSet -> String -> DRuleSet
addDerivedRule rels rel = rel `Set.insert` rels

-- | Consumes a list of summaries (sums).  Returns a list of derived relation names.
addSummaryToSymbols :: RuleDict -> DRuleSet -> DerivationSummary -> Maybe DRuleSet
addSummaryToSymbols rules rels sum =
    case propName $ meta sum of
        Nothing   -> Just rels
        Just name -> if isDefined name
                     then Nothing
                     else Just (rels `addDerivedRule` name)
    where isDefined rel = rels `hasDerivedRule` rel || rules `hasRule` rel
