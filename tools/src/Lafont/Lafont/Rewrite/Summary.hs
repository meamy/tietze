{- HLINT ignore RewritePreamble "Use newtype instead of data" -}

-- | This module provides data-types to summarize derivations, and functions to convert
-- summarizations into rules.

module Lafont.Rewrite.Summary where

import Lafont.Common
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Abstract Summaries of a Derivation.

-- | Maintains metadata about a derivation.
data RewritePreamble = RewritePreamble { propName :: Maybe String
                                       } deriving (Show,Eq)

-- | A description of a derivation that abstracts away all proof details (rewrites).
data DerivationSummary = DerivationSummary { meta :: RewritePreamble
                                           , initial :: MonWord
                                           , final :: MonWord
                                           } deriving (Eq,Show)

-----------------------------------------------------------------------------------------
-- * Functions to Further Abstract Derivations as Rules.

-- | Consumes a summary (sum). Returns a new derived rule which meets the specifications
-- of sum. Requires that sum is named.
createSummaryRule :: DerivationSummary -> RewriteRule
createSummaryRule sum = RewriteRule lhs rhs eqn from
    where lhs  = initial sum
          rhs  = final sum
          eqn  = False
          from = propName (meta sum)

-- | Consumes a summary (sum) and a dictionary of rules (rules). If sum is unnamed, then
-- the rules is unchanged. If sum is named, and the name does not appear in rules, then a
-- rule r is created from sum and a new dictionary is returned, obtained by adding r to
-- rules. If sum is named, and the name appears in rules, then nothing is returned.
addSummaryToRules :: DerivationSummary -> RuleDict -> Maybe RuleDict
addSummaryToRules sum rules =
    case propName $ meta sum of
        Nothing   -> Just rules
        Just name -> if rules `hasRule` name
                     then Nothing
                     else Just (rules `addRule` (name, createSummaryRule sum))
