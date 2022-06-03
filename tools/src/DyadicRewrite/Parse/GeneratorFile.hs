-- | Implements a parser for generator files.

module DyadicRewrite.Parse.GeneratorFile where

import Data.Maybe
import Data.Either
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

-- | Errors unique to generator file parsing.
data GenFileError = MissingSemModel
                  | UnknownSemModel String
                  | InvalidGenName
                  | InvalidGenSem Int String
                  deriving (Eq)

instance Show GenFileError where
    show MissingSemModel         = "Semantics model not provided."
    show (UnknownSemModel model) = "Unknown semantic model (" ++ model ++ ")."
    show InvalidGenName          = "Generator name started with invalid symbol."
    show (InvalidGenSem n msg)   = "Invalid semv at " ++ (show n) ++ " (" ++ msg ++ ")."

-- | Errors returned during generator file parsing.
type GFPError = Either ParserError GenFileError

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

-- | A function used to parse a value given a semantic model. Takes as input a textual
-- representation of the semantic value. Returns either a parsing error (as a string) or
-- a semantic model value.
type SemParser a = (String -> Either String a)

-- | Consumes a semantic model parser (parseSem) and a single line of a generator file
-- (str). Attempts to parse a generator and its semantics model from str. There are three
-- possible return values:
-- 1a. If str does not parse then Nothing is returned.
-- 1b. If str parses as <ID> := <SEM> and <SEM> does not parse then nothing is returned.
-- 2.  If str parses as <ID> then (<ID>, Nothing) is returned.
-- 3.  If str parses as <ID> := <SEM> and <SEM> parses as v, then (<ID>, v) is returned.
-- Requires that strToSem consumes the entire string, and supports both leading and
-- trailing whitespace
parseGenerator :: SemParser a -> String -> Either GFPError (String, Maybe a)
parseGenerator parseSem str =
    case (parseId (snd (trimSpacing str))) of
        Just (id, maybeSemStr) -> case (parseSep ":=" maybeSemStr) of
            Just semStr -> case (parseSem semStr) of
                Left err  -> Left (Right (InvalidGenSem (getErrPos str semStr) err))
                Right sem -> Right (id, Just sem)
            Nothing -> let trimmed = (snd (trimSpacing maybeSemStr))
                       in if (trimmed == "")
                          then Right (id, Nothing)
                          else Left (Left (UnexpectedSymbol (getErrPos str trimmed)))
        Nothing -> Left (Right InvalidGenName)
