-- | This module provides tools to search for derivations of a bounded length.

module Tietze.Edit.Derive
  ( DeriveLog
  , DOption
  , deriveRule
  , newDeriveLog
  , rulesToOptions
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Set as Set
import qualified Data.Map as Map

import Tietze.Common (MonWord)
import Tietze.Rewrite.Common
  ( RuleDir (..)
  , reverseDir
  )
import Tietze.Rewrite.Lookup
  ( RuleDict
  , foldRules
  )
import Tietze.Rewrite.Rules
  ( Rewrite (..)
  , RewriteRule (..)
  , applyRewrite
  , checkRewrite
  )

-----------------------------------------------------------------------------------------
-- * Preprocessing.

-- | A potential rewrite rule.
type DOption = (String, RewriteRule, RuleDir)

-- | Helper function for rulesToOptions, used to fold over the possible rules, and
-- identify the rules which are allowed.
rulesToOptionsImpl :: Set.Set String -> (String, RewriteRule) -> [DOption] -> [DOption]
rulesToOptionsImpl disallowed (name, rule) opts
    | Set.member name disallowed = opts
    | not $ equational rule      = def
    | otherwise                  = (name, rule, R2L):def
    where def = (name, rule, L2R):opts

-- | Extracts a list of valid rewrite rules, from a rule dictionary, together with a set
-- of disallowed rewrite rules. For example, if one wishes to find a derivation which
-- eliminates the rewrite rule r from u to v, then the rule r should be disallowed.
rulesToOptions :: RuleDict -> Set.Set String -> [DOption]
rulesToOptions rules disallowed = foldRules f [] rules
    where f = rulesToOptionsImpl disallowed

-----------------------------------------------------------------------------------------
-- * Option Analysis.

-- Associates a rewrite application with a rule name.
type NRewrite = (String, Rewrite)

-- | Conditionally reverses the direction of a rule.
handleReverse :: Bool -> RuleDir -> RuleDir
handleReverse True  rule = reverseDir rule
handleReverse False rule = rule

-- | Implementation details for optionsToRewrites.
optionsToRewritesImpl :: Bool -> [Int] -> MonWord -> DOption -> [NRewrite] -> [NRewrite]
optionsToRewritesImpl _   []            _    _                     acc = acc
optionsToRewritesImpl rev (pos:indices) word opt@(name, rule, dir) acc
    | checkRewrite word rw = (name, rw):rws
    | otherwise            = rws
    where rw  = Rewrite rule pos $ handleReverse rev dir
          rws = optionsToRewritesImpl rev indices word opt acc

-- | Helper method to compute a list of possible rewrite applications, from a list of
-- rewrite options, and a string. The rev flag indicates whether or not rule directions
-- should be inverted. This flag should be true for backwards searches, and false for
-- forward searches.
optionsToRewrites :: Bool -> [DOption] -> MonWord -> [NRewrite]
optionsToRewrites rev opts word = foldr f [] opts
    where f = optionsToRewritesImpl rev [0..(length word)] word

-----------------------------------------------------------------------------------------
-- * Pruning.

-- | Data structure to maintain proof states which have been previously encountered.
data DeriveLog = DeriveLog (Map.Map MonWord Int) Int deriving (Eq, Show)

-- | Creates an empty derivation log. The cutoff argument specifies how many entries may
-- be cached in the log. If the cutoff is 0, then no caching is allowed. If the cutoff is
-- (-1), then the cache is unbounded. If the cache is some other value n, then at most n
-- elements are allowed in the cache. Elements are never evicted.
newDeriveLog :: Int -> DeriveLog
newDeriveLog cutoff = DeriveLog Map.empty cutoff

-- | A helper function to determine if a word at a given depth already appears in a
-- derivation log.
isNewDerivation :: MonWord -> Int -> DeriveLog -> Bool
isNewDerivation word bnd (DeriveLog log _) =
    case Map.lookup word log of
        Just n  -> n < bnd
        Nothing -> True

-- | Helper function to add a new record to a derivation log. The word, the current
-- bound, and the previous derivation log are taken as inputs. If the caching cutoff has
-- not been reached, then the record is added to the log, and a new log is returned.
-- Otherwise, the old log is returned.
logDerivation :: MonWord -> Int -> DeriveLog -> DeriveLog
logDerivation word bnd old@(DeriveLog log cutoff)
    | cutoff == 0                          = old
    | cutoff < 0 || Map.size nlog < cutoff = DeriveLog nlog cutoff
    | otherwise                            = old
    where nlog = Map.insert word bnd log

-----------------------------------------------------------------------------------------
-- * Derivation Search

-- | Helper function for deriveRule. This function takes the list of valid rewrite
-- applications (as determined by optionsToRewrites), and performs a depth-first search
-- over the possible derivations. If a solution is not found within the depth bound, then
-- the updated derivation log is returned. Otherwise, the list of named rewrites is
-- returned.
--
-- See findMeet for more details on the arguments.
drBranch :: DeriveLog -> [DOption] -> MonWord -> MonWord -> Int -> [NRewrite]
                    -> Either DeriveLog [NRewrite]
drBranch log _    _    _   _     []               = Left log
drBranch log opts goal cur bnd (nrw@(_, rw):nrws) =
    case deriveRule log opts goal (applyRewrite cur rw) (bnd - 1) of
        Left nlog   -> drBranch nlog opts goal cur bnd nrws
        Right steps -> Right $ nrw:steps

-- | This function searches for a derivation from the left-hand side of a rule (cur) to
-- the right-hand side of a rule (goal). To avoid backtracking, this function also takes
-- as input a DeriveLog, which remembers some of the strings derived from previous
-- branches, both in this iteration and in previous iterations. The rewrites used during
-- this search are determined by the list of derivation options, as previous returned by
-- a call to rulesToOptions (opts). To ensure that the search terminates, a depth bound
-- is also taken as a parameter (bnd). The depth bound determines how many steps may
-- appear in a single derivation.
--
-- If a derivation can be found within the constraints of opts and bnd, then the list of
-- named rewrites is returned. Otherwise, the updated derivation log is returned.
deriveRule :: DeriveLog -> [DOption] -> MonWord -> MonWord -> Int
                        -> Either DeriveLog [NRewrite]
deriveRule log opts goal cur bnd
    | goal == cur                 = Right []
    | bnd == 0                    = Left log
    | isNewDerivation cur bnd log = drBranch nlog opts goal cur bnd rws
    | otherwise                   = Left log
    where rws  = optionsToRewrites False opts cur
          nlog = logDerivation cur bnd log
