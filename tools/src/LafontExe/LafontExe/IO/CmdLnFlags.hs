-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module LafontExe.IO.CmdLnFlags
  ( configFlags
  , def
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import System.Console.CmdArgs
  ( Data
  , Typeable
  , (&=)
  , argPos
  , def
  , typ
  , help
  , typFile
  )

-------------------------------------------------------------------------------
-- * Input/Output Flags.

-- | Returns the annotations for a yaml configuration argumrnt in the specified
-- argument position.
configFlags :: Int -> String
configFlags pos = def &= typ "YamlConf" &= argPos pos
