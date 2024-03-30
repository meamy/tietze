-- | Internals for Semantics. Enables unit testing.

module Tietze.Parse.Internal.Semantics (tokenizeByIntIncl) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import           Data.Bifunctor
import           Tietze.Maybe
import           Tietze.Parse.Common
import           Tietze.Parse.DelimLists

-----------------------------------------------------------------------------------------
-- * Products Mod P Semantics: General Interpretation Framework

-- | Consumes an inclusion from integers to type a (incl). Returns a tokenizer to parse
-- integers under the image of incl.
tokenizeByIntIncl :: (Int -> a) -> Tokenizer a
tokenizeByIntIncl incl str = maybeApply (parseInt str) (Data.Bifunctor.first incl)
