-- | Command-line parser for the automated derivation tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module DeriveRule.CmdLn
  ( DeriveRule(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import TietzeExe.IO.CmdLnFlags
  ( configFlags
  , cutoffFlag
  , def
  , relnameFlags
  )
import TietzeExe.IO.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )
import Tietze.Edit.EIRules (EIQueryType(..))

-------------------------------------------------------------------------------
-- * Argument Data Type.

data DeriveRule = DeriveRule { configs :: String
                             , relname :: String
                             , cutoff  :: Int
                             } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

deriveRule :: DeriveRule
deriveRule = DeriveRule { configs = configFlags 0
                        , relname = relnameFlags 1
                        , cutoff  = cutoffFlag 30000
                        }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO DeriveRule
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Automated Derivation Tool"
          desc  = "A command-line tool to search for derivations of rules."
          ctors = [deriveRule]
