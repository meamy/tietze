
-- | Provides the typing data required to log generators.

module LafontExe.Logging.Generators where

import Lafont.Common

-----------------------------------------------------------------------------------------
-- * Monoidal.

instance Display () where
    display _ = "()"
