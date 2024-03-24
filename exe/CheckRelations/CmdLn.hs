-- | Command-line parser for the Relation File Validator.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CheckRelations.CmdLn
  ( CheckRelations(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import TietzeExe.IO.CmdLnFlags (configFlags)
import TietzeExe.IO.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data CheckRelations = CheckRelations { configs :: FilePath
                                     } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

checkRelations :: CheckRelations
checkRelations = CheckRelations { configs = configFlags 0 }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO CheckRelations
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Relation File Validator"
          desc  = "A command-line tool to validate relation files."
          ctors = [checkRelations]
