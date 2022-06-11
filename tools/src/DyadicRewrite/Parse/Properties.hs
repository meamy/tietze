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
