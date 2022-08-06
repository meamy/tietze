-- | This module provides types and functions for string rewrite rules. In particular,
-- the RewriteRule type describes a rule in a rewrite system and the Rewrite type
-- describes the application of a rewrite rule to a specific string.

module Lafont.Rewrite.Rules where

import Data.Maybe
import Lafont.Common

-----------------------------------------------------------------------------------------
-- * RewriteRules.

-- | A rewrite rule in a rewrite system is a tuple of the form (lhs, rhs) and is usually
-- denoted lhs → rhs (we refer to u → v as a production rule as it produces the word xvy
-- from the word xuy for any words x and y). A rule is said to be **equational** if it is
-- symmetric (that is, both lhs → rhs and rhs → lhs are valid). A rule u →* u' may also
-- be **derivedFrom** a proof of the form u → v1 → v2 → ... → vk → u', where each
-- intermediate rule is valid. In this case, the derivedFrom field is assigned the name
-- of the proof.
data RewriteRule = RewriteRule { lhs         :: MonWord
                               , rhs         :: MonWord
                               , equational  :: Bool
                               , derivedFrom :: Maybe String
                               } deriving (Show,Eq)

-- | Consumes a rule. Returns true if the rule is derived.
isDerivedRule :: RewriteRule -> Bool
isDerivedRule rule = isJust (derivedFrom rule)

-- | Consumes a monoidal word and the lhs of a production rule: lhs → rhs. True is
-- returned if the rule is applicable at index .
doesRewriteTermMatch :: MonWord -> MonWord -> Bool
doesRewriteTermMatch _   []  = True
doesRewriteTermMatch []  _   = False
doesRewriteTermMatch str lhs = head str == head lhs && match
    where match = doesRewriteTermMatch (tail str) (tail lhs)

-- | Consumes a monoidal word, a rewrite rule, and a boolean flag indicating if the rule
-- is to be applied from left-to-right. Returns true if rule matches a prefix of the
-- monoidal world.
checkRewriteRule :: MonWord -> RewriteRule -> Bool -> Bool
checkRewriteRule str rule True  = doesRewriteTermMatch str (lhs rule)
checkRewriteRule str rule False = doesRewriteTermMatch str (rhs rule)

-- | Consumes a monoidal word, together with the lhs and rhs of a production rule:
-- lhs → rhs. Returns the monoidal word obtained by applying the production rule at
-- index. Assumes that doesRewriteTermMatch is true.
applyProductionRule :: MonWord -> MonWord -> MonWord -> MonWord
applyProductionRule str []  []  = str
applyProductionRule str []  rhs = head rhs : applyProductionRule str [] (tail rhs)
applyProductionRule str lhs rhs = applyProductionRule (tail str) (tail lhs) rhs

-- | Consumes a monoidal word, a rewrite rule, and a boolean flag indicating if the rule
-- is to be applied from left-to-right. Returns the string obtained by applying the
-- rewrite rule. Assumes that checkRewriteRule is true. 
applyRewriteRule :: MonWord -> RewriteRule -> Bool -> MonWord
applyRewriteRule str rule True  = applyProductionRule str (lhs rule) (rhs rule)
applyRewriteRule str rule False = applyProductionRule str (rhs rule) (lhs rule)

-----------------------------------------------------------------------------------------
-- * Rewrites.

-- | Applies a rewrite rule at the specified position, in the specified direction.
data Rewrite = Rewrite { rule :: RewriteRule
                       , pos :: Int
                       , isLhsToRhs :: Bool
                       } deriving (Show,Eq)

-- | Consumes a monoidal word and a rewrite. Returns true if rule matches at the position
-- indicated by the rewrite.
checkRewrite :: MonWord -> Rewrite -> Bool
checkRewrite str rw = impl str (pos rw)
    where impl substr n = if n == 0
                          then checkRewriteRule substr (rule rw) (isLhsToRhs rw)
                          else not (null substr) && impl (tail substr) (n - 1)

-- | Consumes a monoidal word and a rewrite. Returns the string obtained by applying the
-- rewrite. Assumes that checkRewrite is true. 
applyRewrite :: MonWord -> Rewrite -> MonWord
applyRewrite str rw = impl str (pos rw)
    where impl substr n = if n == 0
                          then applyRewriteRule substr (rule rw) (isLhsToRhs rw)
                          else head substr : impl (tail substr) (n - 1)
