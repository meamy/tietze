-- | Command-line parser for the dependency graph generation tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module GraphDeps.CmdLn
  ( GraphDeps(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LafontExe.IO.CmdLnFlags
  ( configFlags
  , def
  )
import LafontExe.IO.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data GraphDeps = GraphDeps { configs :: String
                           } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

graphDeps :: GraphDeps
graphDeps = GraphDeps { configs = configFlags 0
                      }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO GraphDeps
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Dependency Graphing Tool"
          desc  = "A command-line tool to graph derivational dependencies."
          ctors = [graphDeps]
