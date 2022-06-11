-- | Properties can be given as a preamble at the start of an input file. This module
-- provides functions to build general-purpose property readers.

module DyadicRewrite.Parse.Properties where

import qualified Data.Map
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Generic Parsing Types.

-- | Function to consume a line a read back a property value of type a.
type PropReader a = (String -> Maybe (a, String))

-- | Function to consume a property value and property container, and attempt to update.
-- Should return nothing if and only if the property is already set.
type PropSetter a b = (a -> b -> Maybe b)

-- | Function to consume a line and update property container with error handling.
type PropUpdater b = (String -> b -> Either ParserError b)

-- | Consumes a property name, property reader (read), and property setter (set). Returns
-- a property updater for propery @[name] by joining together read and set with complete
-- error hanlding.
makePropUpdater :: String -> PropReader a -> PropSetter a b -> PropUpdater b
makePropUpdater name read set str container =
    case (read trimmed) of
        Just (prop, post) -> case (set prop container) of
            Just res -> let lval = (UnexpectedSymbol (getErrPos str post))
                        in branchOnSpacing post lval res
            Nothing -> Left (DuplicateProp name)
        Nothing -> Left (UnexpectedSymbol (getErrPos str trimmed))
    where trimmed = snd (trimSpacing str)

-- | Helper function. Consumes a property name, property reader (read), and property
-- setter (set). Returns a tuple (name, (makePropUpdater name read set)). This will be
-- used as an input to a property parser.
makePropPair :: String -> PropReader a -> PropSetter a b -> (String, PropUpdater b)
makePropPair name read set = (name, updater)
    where updater = makePropUpdater name read set

-----------------------------------------------------------------------------------------
-- * Property Lookup.

-- | Dictionary structure to store all properties and their parsers.
type PropertyDict b = Data.Map.Map String (PropUpdater b)

-- | Creates an empty property dictionary.
empty :: PropertyDict b
empty = Data.Map.empty

-- | Consume a dictionary (dict) and a property pair (name, updater). Returns a new
-- dictionary that maps 
addProp :: PropertyDict b -> (String, PropUpdater b) -> PropertyDict b
addProp dict (name, updater) = Data.Map.insert name updater dict

-- | Consumes a dictionary (dict) and a list of property pairs (props). Adds each element
-- to dict, in the other they appear, according to addProp.
addProps :: PropertyDict b -> [(String, PropUpdater b)] -> PropertyDict b
addProps dict []           = dict
addProps dict (prop:props) = addProps (addProp dict prop) props

-- | Converts a dictionary to the list of seperators it represents. Each key is added to
-- the list, with the prefix @. The string "@" is added to the end of the list as a
-- parsing fallthrough (@ corresponds to an unknown property).
propsToSeps :: PropertyDict b -> [String]
propsToSeps dict = Data.Map.foldrWithKey (\name _ names -> (('@':name):names)) ["@"] dict

-- | Consumes a dictionary (dict) and a property name (dict). Returns the updater in dict
-- which corresponds to name. If no entry exists, then a parsing error is raised.
parseFromPropDict :: PropertyDict b -> String -> String -> b -> Either ParserError b
parseFromPropDict dict name str container =
    case (Data.Map.lookup name dict) of
        Just update -> (update str container)
        Nothing     -> Left (UnknownProp name)
