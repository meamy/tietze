-- | Internals for Semantics. Enables unit testing.

module Lafont.Parse.Internal.Semantics (tokenizeByIntIncl) where

import           Data.Bifunctor
import           Lafont.Maybe
import           Lafont.Parse.Common
import           Lafont.Parse.DelimLists

-----------------------------------------------------------------------------------------
-- * Products Mod P Semantics: General Interpretation Framework

-- | Consumes an inclusion from integers to type a (incl). Returns a tokenizer to parse
-- integers under the image of incl.
tokenizeByIntIncl :: (Int -> a) -> Tokenizer a
tokenizeByIntIncl incl str = maybeApply (Data.Bifunctor.first incl) (parseInt str)
