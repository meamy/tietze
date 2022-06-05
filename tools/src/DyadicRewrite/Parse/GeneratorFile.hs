-- | Implements a parser for generator files.

module DyadicRewrite.Parse.GeneratorFile where

import Data.Maybe
import Data.Either
import DyadicRewrite.Generators.Semantics
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

-- | Errors unique to generator file parsing.
data GenFileError = MissingSemModel
                  | UnknownSemModel String
                  | InvalidGenName
                  | InvalidGenSem Int String
                  | DuplicateGenName String
                  deriving (Eq)

instance Show GenFileError where
    show MissingSemModel         = "Semantics model not provided."
    show (UnknownSemModel model) = "Unknown semantic model (" ++ model ++ ")."
    show InvalidGenName          = "Generator name started with invalid symbol."
    show (InvalidGenSem n msg)   = "Invalid semv at " ++ (show n) ++ " (" ++ msg ++ ")."
    show (DuplicateGenName name) = "Duplicate generator name (" ++ name ++ ")."

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
-- trailing whitespace.
parseGenerator :: SemParser a -> String -> Either GFPError (String, Maybe a)
parseGenerator parseSem str =
    case (parseId (snd (trimSpacing str))) of
        Just (id, defStr) -> case (parseSep ":=" defStr) of
            Just semStr -> case (parseSem semStr) of
                Left err  -> Left (Right (InvalidGenSem (getErrPos str semStr) err))
                Right sem -> Right (id, Just sem)
            Nothing -> let lval = Left (UnexpectedSymbol (getErrPos str defStr))
                           rval = (id, Nothing)
                       in branchOnSpacing defStr lval rval
        Nothing -> Left (Right InvalidGenName)

-- | Consumes a semantic model parser (parseSem), a partial map of generators (dict), and
-- a single line of a generator file (str). Attemps to call (parseGenerator parseSem str)
-- and then add the result to dict. If parseGenerator fails, then the error is forwarded.
-- If the generator name is a duplicate, then a DuplicateGenName error is returned. If no
-- errors occur, then the ID and semantic value returned by parseGenerator are added to
-- dict. The resulting dict is returned.
updateGenerators :: SemParser a -> GenDict a -> String -> Either GFPError (GenDict a)
updateGenerators parseSem dict str =
    case (parseGenerator parseSem str) of
        Left err         -> Left err
        Right (id, semv) -> if (dict `hasGen` id)
                            then Left (Right (DuplicateGenName id))
                            else Right (dict `addGen` (id, semv))
