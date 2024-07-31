-- | Command-line parser for the automated meet-in-the-middle search.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module FindMeet.CmdLn
  ( FindMeet(..)
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

data FindMeet = FindMeet { configs :: String
                         , relname :: String
                         , cutoff  :: Int
                         } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

findMeet :: FindMeet
findMeet = FindMeet { configs = configFlags 0
                    , relname = relnameFlags 1
                    , cutoff  = cutoffFlag 30000
                    }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO FindMeet
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Meet-in-the-Middle Search"
          desc  = "A command-line tool to search for the meets of word pairs."
          ctors = [findMeet]
