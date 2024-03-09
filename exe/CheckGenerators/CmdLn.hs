-- | Command-line parser for the Generator File Validator.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CheckGenerators.CmdLn
  ( CheckGenerators(..)
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

data CheckGenerators = CheckGenerators { configs :: FilePath
                                       } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

checkGenerators :: CheckGenerators
checkGenerators = CheckGenerators { configs = configFlags 0 }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO CheckGenerators
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Generator File Validator"
          desc  = "A command-line tool to validate generator files."
          ctors = [checkGenerators]
