-- | Command-line parser for the YAML Parsing Test.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module YamlTest.CmdLn
  ( YamlTest(..)
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

data YamlTest = YamlTest { configs :: FilePath
                         } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

yamlTest :: YamlTest
yamlTest = YamlTest { configs = configFlags 0 }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO YamlTest
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "YAML Parsing Test"
          desc  = "A command-line tool to test the YAML parsing interface."
          ctors = [yamlTest]
