-- | This module provides tools to search for derivations of a bounded length.

module Tietze.Edit.Derive
  ( DeriveLog
  , DOption
  , deriveRule
  , findMeet
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

-- | Associates a rewrite application with a rule name.
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

-----------------------------------------------------------------------------------------
-- * Join Search

-- | Helper function for findMeet. This function takes the list of valid rewrite
-- applications (as determined by optionsToRewrites), and performs a depth-first search
-- over the possible derivations. If a solution is not found within the depth bound, then
-- the updated derivation log is returned. Otherwise, the word at which the left-hand
-- side and right-hand side meet is returned.
--
-- See findMeet for more details on the arguments.
fmBranch :: RuleDir -> DeriveLog -> [DOption] -> DeriveLog -> MonWord -> Int -> [NRewrite]
                    -> Either DeriveLog MonWord
fmBranch _   log _    _     _   _     []               = Left log
fmBranch dir log opts goals cur bnd (nrw@(_, rw):nrws) =
    case findMeet dir log opts goals (applyRewrite cur rw) (bnd - 1) of
        Left nlog -> fmBranch dir nlog opts goals cur bnd nrws
        meet      -> meet

-- | The function searches for a meet between two words u and v. A meet is some word w
-- such that w is derivable from both u and and v is derivable from w. If a path of
-- length 2n exists from u to v, then a meet between u and v exists at depth n. Since
-- increasing the depth of a derivation can increase the number of possible derivations
-- exponentially, then a meet between u and v usually be found in less time than
-- performing a forward search from u to v.
--
-- The function takes as input whether the search is forward is backwards (dir), a list
-- of strings derived in previous iterations (logs), a list of possible rewrites (opts,
-- as computed by rulesToOptions), a list of strings which are known to be derivable from
-- the other side (goals), the word from which the search is beginning (cur), and the
-- maximum derivation depth for this iteration (bnd).
--
-- This function is meant to be used as follows. First, a DeriveLog is created for both
-- the left-hand side and the right-hand side. Then findMeet is applied to both sides, in
-- an alternating fashion, with bnd increasing after each round of searches.
findMeet :: RuleDir -> DeriveLog -> [DOption] -> DeriveLog -> MonWord -> Int
                    -> Either DeriveLog MonWord
findMeet dir log opts goals cur bnd
    | not $ isNewDerivation cur (-1) goals = Right cur
    | bnd == 0 && isNew                    = Left nlog
    | bnd == 0                             = Left log
    | isNew                                = fmBranch dir nlog opts goals cur bnd rws
    | otherwise                            = Left log
    where isNew = isNewDerivation cur bnd log
          rws   = optionsToRewrites (dir == R2L) opts cur
          nlog  = logDerivation cur bnd log
