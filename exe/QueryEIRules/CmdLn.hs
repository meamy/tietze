-- | Command-line parser for the EI Rule Querying Tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QueryEIRules.CmdLn
  ( QueryEIRules(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import TietzeExe.IO.CmdLnFlags
  ( configFlags
  , def
  , eiPolicyFlags
  , leftInvFlags
  , symbolFlags
  )
import TietzeExe.IO.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )
import Lafont.Edit.EIRules (EIQueryType(..))

-------------------------------------------------------------------------------
-- * Argument Data Type.

data QueryEIRules = QueryEIRules { configs :: String
                                 , symbols :: String
                                 , leftInv :: Bool
                                 , policy  :: EIQueryType
                                 } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

queryEIRules :: QueryEIRules
queryEIRules = QueryEIRules { configs = configFlags 0
                            , symbols = symbolFlags 1
                            , leftInv = leftInvFlags def
                            , policy  = eiPolicyFlags NoDefault
                            }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO QueryEIRules
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "EIRule Query Tool"
          desc  = "A command-line tool to query EI rules for a given symbol."
          ctors = [queryEIRules]
