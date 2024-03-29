-- | This module provides types and functions for string rewrite rules. In particular,
-- the RewriteRule type describes a rule in a rewrite system and the Rewrite type
-- describes the application of a rewrite rule to a specific string.

module Tietze.Rewrite.Rules
  ( RewriteRule (..)
  , Rewrite (..)
  , RuleSource (..)
  , applyRewrite
  , applyRewriteRule
  , checkRewrite
  , checkRewriteRule
  , formatRewrite
  , isDerivedRule
  , showRewrite
) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Common (MonWord)
import Tietze.Rewrite.Common
  ( RuleDir (..)
  , RulePos
  )
import           Tietze.Rewrite.Internal.Rules

-----------------------------------------------------------------------------------------
-- * RewriteRules.

-- | Differentiates between primitive relations and derived relations.
data RuleSource = Primitive String
                | Derived (Maybe String)
                deriving (Show, Eq)

-- | A rewrite rule in a rewrite system is a tuple of the form (lhs, rhs) and is usually
-- denoted lhs → rhs (we refer to u → v as a production rule as it produces the word xvy
-- from the word xuy for any words x and y). A rule is said to be **equational** if it is
-- symmetric (that is, both lhs → rhs and rhs → lhs are valid). A rule u →* u' may also
-- be **derivedFrom** a proof of the form u → v1 → v2 → ... → vk → u', where each
-- intermediate rule is valid. In this case, the derivedFrom field is assigned to type
-- Derived, together with the name of the proof. Otherwise, the derivedFrom field is set
-- to Primitive.
data RewriteRule = RewriteRule { lhs         :: MonWord
                               , rhs         :: MonWord
                               , equational  :: Bool
                               , derivedFrom :: RuleSource
                               } deriving (Show, Eq)

-- | Consumes a rule. Returns true if the rule is derived.
isDerivedRule :: RewriteRule -> Bool
isDerivedRule rule =
    case derivedFrom rule of
        Primitive _ -> False
        Derived _   -> True

-- | Consumes a monoidal word, a rewrite rule, and a boolean flag indicating if the rule
-- is to be applied from left-to-right. Returns true if rule matches a prefix of the
-- monoidal word.
checkRewriteRule :: MonWord -> RewriteRule -> RuleDir -> Bool
checkRewriteRule str rule L2R = doesRewriteTermMatch str (lhs rule)
checkRewriteRule str rule R2L = doesRewriteTermMatch str (rhs rule)

-- | Consumes a monoidal word, a rewrite rule, and a boolean flag indicating if the rule
-- is to be applied from left-to-right. Returns the string obtained by applying the
-- rewrite rule. Assumes that checkRewriteRule is true.
applyRewriteRule :: MonWord -> RewriteRule -> RuleDir -> MonWord
applyRewriteRule str rule L2R = applyProductionRule str (lhs rule) (rhs rule)
applyRewriteRule str rule R2L = applyProductionRule str (rhs rule) (lhs rule)

-----------------------------------------------------------------------------------------
-- * Rewrites.

-- | Applies a rewrite rule at the specified position, in the specified direction.
data Rewrite = Rewrite RewriteRule RulePos RuleDir deriving (Show, Eq)

-- | Helper method to generate rewrite strings.
formatRewrite :: String -> Bool -> RuleDir -> Int -> String
formatRewrite name applied dir pos = appstr ++ name ++ dirstr ++ show pos
    where appstr = if applied then "!apply " else ""
          dirstr = if dir == L2R then " → " else " ← "

-- | Takes as input the name of a relation, and a rewrite using said relation. Returns a
-- textual representation of the rewrite, as in a derivation file.
showRewrite :: String -> Rewrite -> String
showRewrite name (Rewrite rule pos dir) = formatRewrite name applied dir pos
    where applied = isDerivedRule rule

-- | Consumes a monoidal word and a rewrite. Returns true if rule matches at the position
-- indicated by the rewrite.
checkRewrite :: MonWord -> Rewrite -> Bool
checkRewrite str (Rewrite rule pos dir) = impl str pos
    where impl substr n = if n == 0
                          then checkRewriteRule substr rule dir
                          else (not . null) substr && impl (tail substr) (n - 1)

-- | Consumes a monoidal word and a rewrite. Returns the string obtained by applying the
-- rewrite. Assumes that checkRewrite is true.
applyRewrite :: MonWord -> Rewrite -> MonWord
applyRewrite str (Rewrite rule pos dir) = impl str pos
    where impl substr n = if n == 0
                          then applyRewriteRule substr rule dir
                          else head substr : impl (tail substr) (n - 1)
