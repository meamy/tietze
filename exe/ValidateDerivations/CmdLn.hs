-- | Command-line parser for the Derivational Proof Checker.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ValidateDerivations.CmdLn
  ( ValidateDerivations(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LafontExe.IO.CmdLnFlags (configFlags)
import LafontExe.IO.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data ValidateDerivations =
    ValidateDerivations { configs :: FilePath
                        } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

validateDerivations :: ValidateDerivations
validateDerivations = ValidateDerivations { configs = configFlags 0 }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO ValidateDerivations
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Derivational Proof Checker"
          desc  = "A command-line tool to validate derivational proofs."
          ctors = [validateDerivations]
