-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module LafontExe.IO.CmdLnFlags
  ( configFlags
  , def
  , eiPolicyFlags
  , leftInvFlags
  , relnameFlags
  , styleFlags
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
configFlags pos = def &= typ "YAML_CONF" &= argPos pos

-- | Returns the annotations for a yaml style argument.  The default value is
-- taken as an argument, since flags are impure.
styleFlags :: Maybe String -> Maybe String
styleFlags x = x &= typ "YAML_STYLE"

-------------------------------------------------------------------------------
-- * Generator/Relation Specification Flags.

-- | Returns the annotations for a relation selection in the specified argument
-- position.
relnameFlags :: Int -> String
relnameFlags pos = def &= typ "REL" &= argPos pos

-- | Returns the annotations for a generation selection in the specified
-- argument position.
symbolFlags :: Int -> String
symbolFlags pos = def &= typ "SYMBS" &= argPos pos

-------------------------------------------------------------------------------
-- * Policy Flags.

-- | Returns the annotation for a flag which indicates that all inverse
-- elements should appear on the left. The default value is taken as an
-- argument, since flags are impure.
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
