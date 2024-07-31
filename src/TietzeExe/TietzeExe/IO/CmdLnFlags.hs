-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module TietzeExe.IO.CmdLnFlags
  ( configFlags
  , cutoffFlag
  , def
  , eiPolicyFlags
  , leftInvFlags
  , relnameFlags
  , styleFlags
  , symbolFlags
  , tsourcesFlags
  ) where

-----------------------------------------------------------------------------------------
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
import Tietze.Edit.EIRules (EIQueryType(..))

-----------------------------------------------------------------------------------------
-- * Input/Output Flags.

-- | Returns the annotations for a yaml configuration argument in the specified argument
-- position.
configFlags :: Int -> String
configFlags pos = def &= typ "YAML_CONF" &= argPos pos

-- | Returns the annotations for a yaml style argument.  The default value is
-- taken as an argument, since flags are impure.
styleFlags :: Maybe String -> Maybe String
styleFlags x = x &= typ "YAML_STYLE"

-----------------------------------------------------------------------------------------
-- * Generator/Relation Specification Flags.

-- | Returns the annotations for a relation selection in the specified argument position.
relnameFlags :: Int -> String
relnameFlags pos = def &= typ "REL" &= argPos pos

-- | Returns the annotations for a generation selection in the specified argument
-- position.
symbolFlags :: Int -> String
symbolFlags pos = def &= typ "SYMBS" &= argPos pos

-----------------------------------------------------------------------------------------
-- * Policy Flags.

-- | Returns th eannotation for a flag which indicates the source dependencies in a
-- proof, as indicated by type rather than name. The default value is taken as an
-- argument, since flags are impure.
tsourcesFlags :: String -> String
tsourcesFlags x = x &= help helpMsg
    where line1   = "Expects a comma-deliminated list of derivation types."
          line2   = "Compute a cone c from all derivations of the given type."
          line3   = "If c is non-empty, then further computation is restricted to c."
          helpMsg = line1 ++ " " ++ line2 ++ " " ++ line3

-- | Returns the annotation for a flag which indicates that all inverse elements should
-- appear on the left. The default value is taken as an argument, since flags are impure.
leftInvFlags :: Bool -> Bool
leftInvFlags x = x &= help helpMsg
    where line1   = "If true, then all inverses appear on the left."
          line2   = "Otherwise, all inverses appear on the right."
          helpMsg = line1 ++ " " ++ line2

-- | Returns the annotations for an EI query type (policy) argument. The default value is
-- taken as an argument, since flags are impure.
eiPolicyFlags :: EIQueryType -> EIQueryType
eiPolicyFlags x = x &= help ("Options: " ++ names)
                    &= typ "QUERY_TYPE"
    where elist = enumFrom $ toEnum 0 :: [EIQueryType]
          slist = map show elist
          names = concat $ intersperse ", " slist

-- | Returns the annotation for a flag which allows an end-user to specify the number of
-- cached results during a tree search. The default value is taken as an argument, since
-- flags are impure.
cutoffFlag :: Int -> Int
cutoffFlag x = x &= help helpMsg
    where line1   = "The number of previous results cached during the search."
          line2   = "A higher cutoff reduces search time but increases memory usage."
          line3   = "To disable caching, set cutoff to 0."
          line4   = "For unbounded caching, set cutoff to a negative value."
          helpMsg = line1 ++ " " ++ line2 ++ " " ++ line3 ++ " " ++ line4
