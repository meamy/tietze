-- | Utilities to prepare derivations for various output formats.

module Tietze.Format.Common
  ( FormattedLine (..)
  , FormattedProof (..)
  , FormattedStep (..)
  , flength
  , formatDerivation
  , formatLine
  , formatProof
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Maybe (listToMaybe)
import Tietze.Common (MonWord (..))
import Tietze.Rewrite.Common (RuleDir (..))
import Tietze.Rewrite.Derivations (Derivation (..))
import Tietze.Rewrite.Rules
  ( Rewrite (..)
  , RewriteRule (..)
  , RuleSource (..)
  , applyRewrite
  )
import Tietze.Rewrite.Summary (DerivationSummary (..))

-----------------------------------------------------------------------------------------
-- * FormattedLine Generation

-- | Decomposes a word into the unchanged symbols, the newly introduced symbols, and the
-- symbols that are to be removed.
data FormattedLine = NoEditLine MonWord
                   | AddThenElimLine MonWord MonWord MonWord
                   | ElimLine MonWord MonWord MonWord
                   | AddLine MonWord MonWord MonWord
                   | AddElimSplitLine MonWord MonWord MonWord MonWord MonWord
                   | ElimAddSplitLine MonWord MonWord MonWord MonWord MonWord
                   | ElimOverAddLine MonWord MonWord MonWord MonWord MonWord
                   | AddOverElimLine MonWord MonWord MonWord MonWord MonWord
                   | ElimAddOverlapLine MonWord MonWord MonWord MonWord MonWord
                   | AddElimOverlapLine MonWord MonWord MonWord MonWord MonWord
                   deriving (Eq, Show)

-- | Handles the case where only a single edit occurs (the other is of length 0).
splitSingleEdit :: MonWord -> (Int, Int) -> (MonWord, MonWord, MonWord)
splitSingleEdit word (pos, len) = (w1, w2, w3)
    where (w1, tmp) = splitAt pos word
          (w2, w3)  = splitAt len tmp

-- | Handles the case where edits are disjoint.
splitDisjointEdit :: MonWord -> (Int, Int) -> (Int, Int)
                             -> (MonWord, MonWord, MonWord, MonWord, MonWord)
splitDisjointEdit word sz@(pos1, len1) (pos2, len2) = (w1, w2, w3, w4, w5)
    where (w1, w2, tmp) = splitSingleEdit word sz
          (w3, w4, w5)  = splitSingleEdit tmp (pos2 - pos1 - len1, len2)

-- | Handles the case where one edit is contained within another edit.
nestedEdit :: MonWord -> (Int, Int) -> (Int, Int)
                      -> (MonWord, MonWord, MonWord, MonWord, MonWord)
nestedEdit word sz@(opos, olen) (ipos, ilen) = (w1, w2, w3, w4, w5)
    where (w1, tmp)    = splitAt opos word
          (inner, w5)  = splitAt olen tmp
          (w2, w3, w4) = splitSingleEdit inner (ipos - opos, ilen)

-- | Handles the case where edits overlap.
overlapEdit :: MonWord -> (Int, Int) -> (Int, Int)
                       -> (MonWord, MonWord, MonWord, MonWord, MonWord)
overlapEdit word (pos1, len1) (pos2, len2) = (w1, w2, w3, w4, w5)
    where difference = pos2 - pos1
          (w1, tmp1) = splitAt pos1 word
          (w2, tmp2) = splitAt difference tmp1
          (w3, tmp3) = splitAt (len1 - difference) tmp2
          (w4, w5)   = splitAt (len2 + difference - len1) tmp3

-- | Case distinctions for formatLine.
formatLineImpl :: MonWord -> (Int, Int) -> (Int, Int) -> FormattedLine
formatLineImpl word asz@(apos, alen) esz@(epos, elen)
    | alen == 0 && elen == 0       = NoEditLine word
    | asz == esz                   = let (x, y, z) = splitSingleEdit word asz
                                     in AddThenElimLine x y z
    | alen == 0                    = let (x, y, z) = splitSingleEdit word esz
                                     in ElimLine x y z
    | elen == 0                    = let (x, y, z) = splitSingleEdit word asz
                                     in AddLine x y z
    | abnd <= epos                 = let (u, v, w, x, y) = splitDisjointEdit word asz esz
                                     in AddElimSplitLine u v w x y
    | ebnd <= apos                 = let (u, v, w, x, y) = splitDisjointEdit word esz asz
                                     in ElimAddSplitLine u v w x y
    | epos <= apos && abnd <= ebnd = let (u, v, w, x, y) = nestedEdit word esz asz
                                     in ElimOverAddLine u v w x y
    | apos <= epos && ebnd <= abnd = let (u, v, w, x, y) = nestedEdit word asz esz
                                     in AddOverElimLine u v w x y
    | epos < apos && ebnd < abnd   = let (u, v, w, x, y) = overlapEdit word esz asz
                                     in ElimAddOverlapLine u v w x y
    | apos < epos && abnd < ebnd   = let (u, v, w, x, y) = overlapEdit word asz esz
                                     in AddElimOverlapLine u v w x y
    | otherwise                    = error "Unhandled line format."
    where abnd = apos + alen
          ebnd = epos + elen

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

-- | Returns the combined length of all MonWords in a list.
sumlength :: [MonWord] -> Int
sumlength = foldr (+) 0 . map length

-- | Returns the length of a formatted line.
flength :: FormattedLine -> Int
flength (NoEditLine w)                    = length w
flength (ElimLine w1 w2 w3)               = sumlength [w1, w2, w3]
flength (AddLine w1 w2 w3)                = sumlength [w1, w2, w3]
flength (ElimAddSplitLine w1 w2 w3 w4 w5) = sumlength [w1, w2, w3, w4, w5]
flength (AddElimSplitLine w1 w2 w3 w4 w5) = sumlength [w1, w2, w3, w4, w5]

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
