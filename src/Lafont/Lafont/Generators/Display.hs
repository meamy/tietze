-- | Provides the typing data required to display generators. Since most generators are
-- built on top of existing types, the Display class data must be provided separately.

module Lafont.Generators.Display (display) where

import           Lafont.Common
import           Lafont.Generators.Algebraic.Internal.ModP
import           Lafont.Generators.Algebraic.Internal.Product
import           Lafont.Generators.Categories
import           Lafont.Generators.QubitGates
import           Lafont.Ring
import           Lafont.String
import qualified Quantum.Synthesis.Matrix                     as QMat
import qualified Quantum.Synthesis.Ring                       as QRing

-----------------------------------------------------------------------------------------
-- * Monoid Semantics.

instance Display () where
    display _ = "()"

-----------------------------------------------------------------------------------------
-- * Clifford(D)+Tof Semantics.

instance Display QRing.Dyadic where
    display d = case reduceDyadic d of
        (QRing.Dyadic a 0) -> show a
        (QRing.Dyadic 0 n) -> "0"
        (QRing.Dyadic a 1) -> show a ++ "/2"
        (QRing.Dyadic a n) -> show a ++ "/2^" ++ show n

displayRow :: (Display a) => [a] -> String
displayRow []      = ""
displayRow [a]     = display a
displayRow (a:row) = display a ++ ", " ++ displayRow row

displayRows :: (Display a) => [[a]] -> String
displayRows []       = ""
displayRows [r]      = "[" ++ displayRow r ++ "]"
displayRows (r:rows) = "[" ++ displayRow r ++ "], " ++ displayRows rows

instance (QMat.Nat n, QMat.Nat m, Display a) => Display (QMat.Matrix n m a) where
    display mat = "[" ++ displayRows (QMat.rows_of_matrix mat) ++ "]"

-----------------------------------------------------------------------------------------
-- * MultModP and AddModP Semantics

instance Display AddInt where
    display (AddInt v) = show v

instance Display MultInt where
    display (MultInt v) = show v

instance (ModP a, Display a) => Display (ArithModP a) where
    display ArithID         = "ID"
    display (ArithModP v 0) = display v
    display (ArithModP v p) = display v ++ " mod " ++ show p

instance (Display a) => Display (ProductType a) where
    display ProductID       = "ID"
    display (ProductVal vs) = "(" ++ formatList display vs ++ ")"
