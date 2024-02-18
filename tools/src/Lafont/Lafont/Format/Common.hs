-- | Utilities to prepare derivations for various output formats.

module Lafont.Format.Common (
    FormattedLine ( .. ),
    FormattedProof ( .. ),
    FormattedStep ( .. ),
    flength,
    formatDerivation,
    formatLine,
    formatProof
) where

import Data.Maybe
import Lafont.Common
import Lafont.Rewrite.Common
import Lafont.Rewrite.Derivations
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * FormattedLine Generation

-- | Decomposes a word into the unchanged symbols, the newly introduced symbols, and the
-- symbols that are to be removed.
data FormattedLine = NoEditLine MonWord deriving (Eq, Show)

-- | Case distinctions for formatLine.
formatLineImpl :: MonWord -> (Int, Int) -> (Int, Int) -> FormattedLine
formatLineImpl word _ _ = NoEditLine word

-- | Helper method to determine the position and length associated to a rewrite rule.
extractRewriteSize :: (Rewrite -> MonWord) -> Maybe Rewrite -> (Int, Int)
extractRewriteSize _    Nothing                     = (0, 0)
extractRewriteSize read (Just rw@(Rewrite _ pos _)) = (pos, length $ read rw)

-- | Returns the target of a rewrite.
extractFromPrev :: Rewrite -> MonWord
extractFromPrev (Rewrite rule _ L2R) = rhs rule
extractFromPrev (Rewrite rule _ R2L) = lhs rule

-- | Returns the result of rewrite.
extractFromNext :: Rewrite -> MonWord
extractFromNext (Rewrite rule _ L2R) = lhs rule
extractFromNext (Rewrite rule _ R2L) = rhs rule

-- | Takes as input the symbols within a line, the rule which was applied to obtain this
-- line (if it is not the first line in a derivation), and the rule which is applied to
-- this line (if it is not the last line in a derivation). Returns a formatted version of
-- the line.
formatLine :: MonWord -> Maybe Rewrite -> Maybe Rewrite -> FormattedLine
formatLine word prev next = formatLineImpl word psize nsize
    where psize = extractRewriteSize extractFromPrev prev
          nsize = extractRewriteSize extractFromNext next

-- | Returns the length of a formatted line.
flength :: FormattedLine -> Int
flength (NoEditLine word) = length word

-----------------------------------------------------------------------------------------
-- * FormattedProof Generation

-- | A fully-expanded step in a derivational proof.
data FormattedStep = FormattedStep RuleSource RuleDir FormattedLine deriving (Eq, Show)

-- | A fully-expanded derivational proof.
data FormattedProof = FormattedProof FormattedLine [FormattedStep] deriving (Eq, Show)

-- | Pattern-matching for the recursive call in formatProofImpl.
formatProofRec :: MonWord -> FormattedStep -> [Rewrite] -> [FormattedStep]
formatProofRec _    step []       = [step]
formatProofRec word step (rw:rws) = step : formatProofImpl word rw rws

-- | Implementation details for formatProof. Generates the sequence of formatted steps.
-- The current rewrite rule is taken as an explicit parameter.
formatProofImpl :: MonWord -> Rewrite -> [Rewrite] -> [FormattedStep]
formatProofImpl word cur@(Rewrite rule _ dir) rws = formatProofRec curword curstep rws
    where curword = applyRewrite word cur
          curline = formatLine curword (Just cur) (listToMaybe rws)
          curstep = FormattedStep (derivedFrom rule) dir curline

-- | Takes as input a word and a sequence of rewrites. If the derivation is valid, then a
-- fully-expanded derivational proof is returned. Otherwise, an error is risen.
formatProof :: MonWord -> [Rewrite] -> FormattedProof
formatProof word [] = FormattedProof initLine []
    where initLine = formatLine word Nothing Nothing
formatProof word (rw:rws) = FormattedProof initLine stepList
    where initLine = formatLine word Nothing $ Just rw
          stepList = formatProofImpl word rw rws

-- | Formats a derivation as a proof. The initial word is taken to be the initial word of
-- the derivation, and the list of rewrites is taken to be the rewrites making up the
-- steps of the derivation.
formatDerivation :: Derivation -> FormattedProof
formatDerivation (Derivation sum rewrites) = formatProof (initial sum) rewrites
