-- | Implements a parser for derivation files.

module Tietze.Parse.DerivationFile
  ( PreDerivation (..)
  , DFPError
  , display
  , preparseDerivationFile
  , parseDerivationFile
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import           Tietze.Common
import           Tietze.Either
import           Tietze.Parse.Common
import           Tietze.Parse.Internal.DerivationFile
import           Tietze.Rewrite.Abstraction
import           Tietze.Rewrite.Lookup
import           Tietze.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

instance Display DerFileError where
    display UnknownRewriteMod         = "Unknown rewrite modifier (a symbol prefixed by !)."
    display InvalidRuleName           = "Rewrite rule name starts with invalid symbol."
    display InvalidRewritePos         = "Expected position at end of rewrite."
    display InvalidRewriteDir         = "Non-equational rewrite rule applied right-to-left."
    display MissingRewriteDir         = "Equational rewrite rule requires derivation direction."
    display (UnknownGenName name)     = "Unknown generator name (" ++ name ++ ")."
    display (UnknownRuleName name)    = "Unknown rewrite rule (" ++ name ++ ")."
    display (UnknownDerivedRule name) = "Unknown derived rule (" ++ name ++ ")."
    display MissingInitialWord        = "Initial word missing in derivation."
    display MissingFinalWord          = "Final word missing in derivation."

-----------------------------------------------------------------------------------------
-- * Derivation Body Parsing.

-- | Consumes a list of known generator names (gens) and the lines of a derivation file
-- including the preamble (lines). If the lines include a preamble section, an initial
-- word section, a rewrite section, and a word string section, with the preamble,
-- initial, and final words valid, then a list of summaries for all derivations in the
-- file are returned. Otherwise, returns a parsing exception.
preparseDerivationFile :: [String] -> [String] -> Int -> DParseRV [PreDerivation]
preparseDerivationFile gens lines num =
    branchRight (preparseDerivation gens lines num)
        (\(pre, rest) -> if isEOFSpacing rest
                         then Right [pre]
                         else let nextNum = num + length lines - length rest
                              in updateRight (preparseDerivationFile gens rest nextNum)
                                             (pre :))

-- | Consumes a dictionary of known rules, including derived rules (rules), a set of
-- derived relation symbols (derived), and the summary of a derivation file (pre). If the
-- body is a valid rewrite section with respect to rules, then a derivation is returned.
-- Otherwise, returns a parsing exception.
parseDerivationFile :: RuleDict -> DRuleSet -> PreDerivation -> DParseRV AbsDerivation
parseDerivationFile rules derived pre =
    updateRight (parseRewriteLines rules derived (unparsed pre) (linenum pre))
                (AbsDerivation (parsed pre))
