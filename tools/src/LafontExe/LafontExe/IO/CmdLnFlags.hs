-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module LafontExe.IO.CmdLnFlags
  ( configFlags
  , def
  , eiPolicyFlags
  , leftInvFlags
  , relnameFlags
  , symbolFlags
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List
  ( concat
  , intersperse
  )
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
import Lafont.Edit.EIRules (EIQueryType(..))

-------------------------------------------------------------------------------
-- * Input/Output Flags.

-- | Returns the annotations for a yaml configuration argument in the specified
-- argument position.
configFlags :: Int -> String
configFlags pos = def &= typ "YamlConf" &= argPos pos

-------------------------------------------------------------------------------
-- * Generator/Relation Specification Flags.

-- | Returns the annotations for a relation selection in the specified argument
-- position.
relnameFlags :: Int -> String
relnameFlags pos = def &= typ "Relation" &= argPos pos

-- | Returns the annotations for a generation selection in the specified
-- argument position.
symbolFlags :: Int -> String
symbolFlags pos = def &= typ "Symbols" &= argPos pos

-------------------------------------------------------------------------------
-- * Policy Flags.

leftInvFlags :: Bool -> Bool
leftInvFlags x = x &= help helpMsg
    where line1   = "If true, then all inverses appear on the left."
          line2   = "Otherwise, all inverses appear on the right."
          helpMsg = line1 ++ " " ++ line2

-- | Returns the annotations for an EI query type (policy) argument. The
-- default value is taken as an argument, since flags are impure.
eiPolicyFlags :: EIQueryType -> EIQueryType
eiPolicyFlags x = x &= help ("Options: " ++ names)
                   &= typ "QUERY_TYPE"
    where elist = enumFrom $ toEnum 0 :: [EIQueryType]
          slist = map show elist
          names = concat $ intersperse ", " slist
