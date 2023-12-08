-- | Command-line parser for the relation inversion tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module InvertRule.CmdLn
  ( InvertRule(..)
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
import Lafont.Edit.EIRules (EIQueryType(..))

-------------------------------------------------------------------------------
-- * Argument Data Type.

data InvertRule = InvertRule { configs :: String
                             , relname :: String
                             , leftInv :: Bool
                             , policy  :: EIQueryType
                             } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

invertRule :: InvertRule
invertRule = InvertRule { configs = configFlags 0
                        , relname = relnameFlags 1
                        , leftInv = leftInvFlags def
                        , policy  = eiPolicyFlags NoDefault
                        }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO InvertRule
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "Rule Inversion Tool"
          desc  = "A command-line tool to invert relations via EIRules."
          ctors = [invertRule]
