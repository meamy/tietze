-- | Internals for Properties. Enables unit testing.

module Tietze.Parse.Internal.Properties
  ( PropUpdater
  , PropertyDict
  , PropParser
  , propsToSeps
  , parseFromPropDict
  , parsePropLine
  , parsePreamble
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map            as Map
import           Tietze.Parse.Common

-----------------------------------------------------------------------------------------
-- * Generic Parsing Types.

-- | Function to consume a line and update property container with error handling.
type PropUpdater b = (String -> b -> Either ParserError b)

-----------------------------------------------------------------------------------------
-- * Property Lookup.

-- | Dictionary structure to store all properties and their parsers.
type PropertyDict b = Map.Map String (PropUpdater b)

-- | Converts a dictionary to the list of seperators it represents. Each key is added to
-- the list, with the prefix @. The string "@" is added to the end of the list as a
-- parsing fallthrough (@ corresponds to an unknown property).
propsToSeps :: PropertyDict b -> [String]
propsToSeps = Map.foldrWithKey (\name _ names -> ('@':name):names) ["@"]

-- | Consumes a dictionary (dict) and a property name (dict). Returns the updater in dict
-- which corresponds to name. If no entry exists, then a parsing error is raised.
parseFromPropDict :: PropertyDict b -> String -> String -> b -> Either ParserError b
parseFromPropDict dict name str container =
    case Map.lookup name dict of
        Just update -> update str container
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
    case parseFromSeps seps line of
        Just ("@", post) -> let prop = fst $ splitAtFirst (not . isSpacing) post
                            in Left (UnknownProp prop)
        Just ('@':prop, post) -> case parseFromPropDict dict prop post container of
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
parsePreamble seps dict container (line:lines) num
    | trimmed == "" = parsePreamble seps dict container lines (num + 1)
    | otherwise     = case parsePropLine seps dict container trimmed of
        Left  err        -> Left (num, propCommonErr stripped trimmed err)
        Right (Just res) -> parsePreamble seps dict res lines (num + 1)
        Right Nothing    -> Right (line : lines, num, container)
    where stripped = stripComments line
          (_, trimmed) = trimSpacing stripped
