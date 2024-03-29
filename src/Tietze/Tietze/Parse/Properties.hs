-- | Properties can be given as a preamble at the start of an input file. This module
-- provides functions to build general-purpose property readers.

module Tietze.Parse.Properties
  ( PropUpdater
  , PropertyDict
  , PropParser
  , PropReader
  , PropSetter
  , makePropPair
  , noProps
  , addProp
  , addProps
  , makePreambleParser
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map                         as Map
import           Tietze.Parse.Common
import           Tietze.Parse.Internal.Properties

-----------------------------------------------------------------------------------------
-- * Generic Parsing Types.

-- | Function to consume a line a read back a property value of type a.
type PropReader a = (String -> Maybe (a, String))

-- | Function to consume a property value and property container, and attempt to update.
-- Should return nothing if and only if the property is already set.
type PropSetter a b = (a -> b -> Maybe b)

-- | Consumes a property name, property reader (read), and property setter (set). Returns
-- a property updater for propery @[name] by joining together read and set with complete
-- error hanlding.
makePropUpdater :: String -> PropReader a -> PropSetter a b -> PropUpdater b
makePropUpdater name read set str container =
    case read trimmed of
        Just (prop, post) -> case set prop container of
            Just res -> branchOnSpacing post (UnexpectedSymbol (getErrPos str post)) res
            Nothing -> Left (DuplicateProp name)
        Nothing -> Left (UnexpectedSymbol (getErrPos str trimmed))
    where (_, trimmed) = trimSpacing str

-- | Helper function. Consumes a property name, property reader (read), and property
-- setter (set). Returns a tuple (name, (makePropUpdater name read set)). This will be
-- used as an input to a property parser.
makePropPair :: String -> PropReader a -> PropSetter a b -> (String, PropUpdater b)
makePropPair name read set = (name, updater)
    where updater = makePropUpdater name read set

-----------------------------------------------------------------------------------------
-- * Property Lookup.

-- | Creates an empty property dictionary.
noProps :: PropertyDict b
noProps = Map.empty

-- | Consume a dictionary (dict) and a property pair (name, updater). Returns a new
-- dictionary that maps
addProp :: PropertyDict b -> (String, PropUpdater b) -> PropertyDict b
addProp dict (name, updater) = Map.insert name updater dict

-- | Consumes a dictionary (dict) and a list of property pairs (props). Adds each element
-- to dict, in the other they appear, according to addProp.
addProps :: PropertyDict b -> [(String, PropUpdater b)] -> PropertyDict b
addProps = foldl addProp

-----------------------------------------------------------------------------------------
-- * Property Preamble Parsing.

-- | Helper function. Consumes a dictionary of properties (dict) and an empty container
-- (empty). Returns the parser produced by parsePreamble when called correctly using dict
-- and empty.
makePreambleParser :: PropertyDict b -> b -> PropParser b
makePreambleParser dict = parsePreamble (propsToSeps dict) dict
