-- | Command-line parser for the LaTeX generation tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ToLatex.CmdLn
  ( ToLatex(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LafontExe.IO.CmdLnFlags
  ( configFlags
  , def
  , eiPolicyFlags
  , leftInvFlags
  , relnameFlags
  )
import LafontExe.IO.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data ToLatex = ToLatex { configs :: String
                       } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

toLatex :: ToLatex
toLatex = ToLatex { configs = configFlags 0
                  }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO ToLatex
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "LaTeX Generation Tool"
          desc  = "A command-line tool to convert derivations to LaTeX."
          ctors = [toLatex]
