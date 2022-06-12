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
noProps :: PropertyDict b
noProps = Data.Map.empty

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

-----------------------------------------------------------------------------------------
-- * Property Preamble Parsing.

-- | Function to consume the lines of a file, parse its preamble (a list of properties),
-- and return a property container together with the remaining lines. If parsing fails,
-- then an error is returned.
type PropParser b = ([String] -> Int -> Either (Int, ParserError) ([String], Int, b))

-- | Consumes a list of separators (seps), a dictionary of properties (dict), a container
-- for said properties (container), and a line of a preamble. Attempts to parse a
-- property listed in seps using a parser from dict. If parsing is successful then
-- container is updated and returned. Otherwise, a parsing error is returned. Requires
-- that seps == (propsToSeps dict), that line is free from leading whitespace, and line
-- is free from comments.
parsePropLine :: [String] -> PropertyDict b -> b -> String -> Either ParserError (Maybe b)
parsePropLine seps dict container line =
    case (parseFromSeps seps line) of
        Just ("@", post) -> let prop = fst (splitAtFirst (\c -> not $ isSpacing c) post)
                            in Left (UnknownProp prop)
        Just ('@':prop, post) -> case (parseFromPropDict dict prop post container) of
            Left err  -> Left (propCommonErr line post err)
            Right res -> Right (Just res)
        Just _  -> Left (ImplError "Property not prefixed with @.")
        Nothing -> Right Nothing -- End of preamble.

-- | Consumes a list of separators (seps), a dictionary of properties (dict), a container
-- for said properties (container), and a line of a preamble. Returns a PropParser fn.
-- The function fn attempts to aprse all lines of a preamble using the properties listed
-- in seps and the parsers stored in dict. Parsing terminates when all lines are consumed
-- or a non-property line is reached. If parsing is successful then returns container
-- updated with all properties, and the list of remaining lines. Otherwise, a parsing
-- error is returned. Requires that seps == (propsToSeps dict).
parsePreamble :: [String] -> PropertyDict b -> b -> PropParser b
parsePreamble _    _    container []           num = Right ([], num, container)
parsePreamble seps dict container (line:lines) num =
    case (snd (trimSpacing stripped)) of
        ""   -> parsePreamble seps dict container lines (num + 1)
        text -> case (parsePropLine seps dict container text) of
            Left  err        -> Left (num, (propCommonErr stripped text err))
            Right (Just res) -> parsePreamble seps dict res lines (num + 1)
            Right Nothing    -> Right ((line:lines), num, container)
    where stripped = stripComments line

-- | Helper function. Consumes a dictionary of properties (dict) and an empty container
-- (empty). Returns the parser produced by parsePreamble when called correctly using dict
-- and empty.
makePreambleParser :: PropertyDict b -> b -> PropParser b
makePreambleParser dict empty = parsePreamble (propsToSeps dict) dict empty    
