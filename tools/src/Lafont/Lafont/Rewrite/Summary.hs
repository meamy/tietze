{- HLINT ignore RewritePreamble "Use newtype instead of data" -}

-- | This module provides data-types to summarize derivations, and functions to convert
-- summarizations into rules.

module Lafont.Rewrite.Summary (
    -- Re-exports from internal.
    EqMap,
    DRuleSet,
    -- Exports.
    RewritePreamble ( .. ),
    DerivationSummary ( .. ),
    addDRule,
    defaultEqMap,
    setAsEquational,
    setAsOrientated,
    containsRule,
    isEquationalDerivation,
    isSummaryEquational,
    createSummaryRule,
    nullRuleSet,
    hasDerivedRule,
    addDerivedRule,
    addSummaryToSymbols,
) where

import           Data.Map                        as Map
import           Data.Set                        as Set
import           Lafont.Common
import           Lafont.Rewrite.Internal.Summary
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

-- Returns an empty EqMap.
defaultEqMap :: EqMap
defaultEqMap = EqMap Map.empty

-- | Records that the derivation name is equational in map.
setAsEquational :: EqMap -> String -> EqMap
setAsEquational (EqMap map) name = EqMap $ Map.insert name True map

-- | Records that the derivation name is orientated in map.
setAsOrientated :: EqMap -> String -> EqMap
setAsOrientated (EqMap map) name = EqMap $ Map.insert name False map

-- | Records true if name is recorded in map.
containsRule :: EqMap -> String -> Bool
containsRule (EqMap map) name = name `Map.member` map

-- | Reads an entry from an EqMap.
isEquationalDerivation :: EqMap -> String -> Maybe Bool
isEquationalDerivation (EqMap map) name = name `Map.lookup` map

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
          from = propName $ meta sum

-- | Consumes a rule dictionary, an equationality map (emap) a summary recorded in the
-- map (sum). If the summary is named, then returns a new rule dictionary obtained by
-- recording the summary as a rule (createSummaryRule). Otherwise, the original
-- dictionary is returned.
addDRule :: RuleDict -> EqMap -> DerivationSummary -> RuleDict
addDRule rules emap sum =
    case propName $ meta sum of
        Nothing   -> rules
        Just name -> addRule rules (name, rule)
    where rule = createSummaryRule emap sum

-----------------------------------------------------------------------------------------
-- * Functions to Register Derivations as Relations.

-- | An empty collection of derived relation symbols.
nullRuleSet :: DRuleSet
nullRuleSet = DRuleSet Set.empty

-- | Returns true if a DRuleSet contains a relation symbol.
hasDerivedRule :: DRuleSet -> String -> Bool
hasDerivedRule (DRuleSet rels) rel = rel `Set.member` rels

-- | Adds a relation symbol to a DRuleSet.
addDerivedRule :: DRuleSet -> String -> DRuleSet
addDerivedRule (DRuleSet rels) rel = DRuleSet $ rel `Set.insert` rels

-- | Consumes a list of summaries (sums).  Returns a list of derived relation names.
addSummaryToSymbols :: RuleDict -> DRuleSet -> DerivationSummary -> Maybe DRuleSet
addSummaryToSymbols rules rels sum =
    case propName $ meta sum of
        Nothing   -> Just rels
        Just name -> if isDefined name
                     then Nothing
                     else Just $ rels `addDerivedRule` name
    where isDefined rel = rels `hasDerivedRule` rel || rules `hasRule` rel
