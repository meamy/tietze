-- | Implements a parser for generator files.

module Lafont.Parse.GeneratorFile where

import Lafont.Common
import Lafont.Generators.Semantics
import Lafont.Parse.Common
import Lafont.Parse.Semantics

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

-- | Errors unique to generator file parsing.
data GenFileError = MissingSemModel
                  | UnknownSemModel String
                  | SemModelWOImpl SemModel
                  | InvalidGenName
                  | InvalidGenSem Int String
                  | DuplicateGenName String
                  deriving (Eq,Show)

instance Display GenFileError where
    display MissingSemModel         = "Semantics model not provided."
    display (UnknownSemModel model) = "Unknown semantic model (" ++ model ++ ")."
    display (SemModelWOImpl model)  = (display model) ++ " not implemented."
    display InvalidGenName          = "Generator name started with invalid symbol."
    display (InvalidGenSem pos msg) = "Invalid semv at " ++ (show pos) ++ " (" ++ msg ++ ")."
    display (DuplicateGenName name) = "Duplicate generator name (" ++ name ++ ")."

-- | Errors returned during generator file parsing.
type GFPError = Either ParserError GenFileError

-----------------------------------------------------------------------------------------
-- * Line Parsing Helper Methods.

-- | Helper function to propogation generator file errors from a callee parsing function
-- to a caller parsing function. For example, if an error occurs at index 5 of substr,
-- and if substr appears at index 7 of str, then the error is updated to index 12.
--
-- Note: All cases are stated explicitly, so that adding a new positional error without
-- updating this method will result in a compile-time type error.
propGenErr :: String -> String -> GFPError -> GFPError
propGenErr str substr (Left err)  = Left (propCommonErr str substr err)
propGenErr str substr (Right err) =
    case err of
        MissingSemModel         -> Right MissingSemModel
        (UnknownSemModel model) -> Right (UnknownSemModel model)
        InvalidGenName          -> Right InvalidGenName
        (InvalidGenSem pos msg) -> Right (InvalidGenSem (update pos) msg)
        DuplicateGenName name   -> Right (DuplicateGenName name)
    where update pos = relToAbsErrPos str substr pos

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

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

-----------------------------------------------------------------------------------------
-- * File Parsing Methods.

-- | List of all semantic models as text.
_semModelStrings :: [String]
_semModelStrings = [(display MonoidalSem),
                    (display DyadicTwoSem),
                    (display DyadicThreeSem)]

-- | Consumes all lines of a generator file (lines) and the current line number (num).
-- Attempts to parse the semantic model declaration. If successful, then the semantic
-- model, the number of lines parsed, and all remaining lines are returned. Otherwise, a
-- parsing error is returned along with the line number.
parseSemanticModel :: [String] -> Int -> Either (Int, GFPError) (SemModel, Int, [String])
parseSemanticModel []           num = Left (num, (Right MissingSemModel))
parseSemanticModel (line:lines) num =
    case (cleanLine line) of
        ""   -> (parseSemanticModel lines (num + 1))
        text -> case parseFromSeps _semModelStrings text of
            Just ("Monoidal",  post) -> (check MonoidalSem post text)
            Just ("Dyadic(2)", post) -> (check DyadicTwoSem post text)
            Just ("Dyadic(3)", post) -> (check DyadicThreeSem post text)
            Nothing                  -> Left (num, (Right (UnknownSemModel text)))
    where check sem post text = let lval = (num, Right (UnknownSemModel text))
                                    rval = (sem, num, lines)
                                in branchOnSpacing post lval rval

-- | Consumes a semantic model parser (parseSem) and all lines of a generator file
-- (lines), excluding the semantic model declaration. If the lines are valid, then
-- returns a dictionary of all generators and their semantics. Otherwise, returns a
-- parsing exception.
--
-- Note that the semantic model declaration determines the type a. This means that the
-- semantic model declaration must be parsed independent of this code.
parseGenDict :: SemParser a -> [String] -> Int -> Either (Int, GFPError) (GenDict a)
parseGenDict _        []           _   = Right empty
parseGenDict parseSem (line:lines) num =
    case (parseGenDict parseSem lines (num + 1)) of
        Left  err  -> Left err
        Right dict -> case (snd (trimSpacing stripped)) of
            ""   -> Right dict
            text -> case (updateGenerators parseSem dict text) of
                Left err   -> Left (num, (propGenErr stripped text err))
                Right dict -> Right dict
    where stripped = stripComments line

-----------------------------------------------------------------------------------------
-- * Full Generator File Parsing.

-- | Lifts semantic value types to the return value. This is a cleaner alternative to the
-- type: Either (GenDict SemV1) (Either (GenDict SemV2), (Either SemV3 (Either ...)))).
--
-- In the future, this might also carry on parameters that describe the generator file.
data GenFileSummary = MonoidalGenSummary (GenDict ())
                    | DyadicTwoSummary (GenDict TwoQubitDyadic)
                    | DyadicThreeSummary (GenDict ThreeQubitDyadic)
                    deriving (Eq,Show)

-- | Consumes all lines of a generator file (lines). If the lines are valid, then returns
-- a dictionary of all generators and their semantics, wrapped by their semantic model.
-- Otherwise, returns a parsing exception.
parseGenFileAsDict :: [String] -> Int -> Either (Int, GFPError) GenFileSummary
parseGenFileAsDict lines num =
    case (parseSemanticModel lines 0) of
        Left err                 -> Left err
        Right (sem, semLn, gens) -> let nextLn = semLn + 1 in case sem of
            MonoidalSem -> case (parseGenDict parseMonoidalSem gens nextLn) of
                Left err   -> Left err
                Right dict -> Right (MonoidalGenSummary dict)
            DyadicTwoSem -> case (parseGenDict interpret2QubitCliffordDTofGate gens nextLn) of
                Left err   -> Left err
                Right dict -> Right (DyadicTwoSummary dict)
            DyadicThreeSem -> case (parseGenDict interpret3QubitCliffordDTofGate gens nextLn) of
                Left err   -> Left err
                Right dict -> Right (DyadicThreeSummary dict)
            otherwise -> Left (semLn, Right (SemModelWOImpl sem))

-- | A GenFileSummary carries the type data of the underlying semantic model. This
-- function allows all semantic data to be stripped away, returning instead a list of
-- generator symbols. Consumes all lines of a generator file (lines). If the lines are
-- valid, then returns a list of all generator symbols. Otherwise, returns a parsing
-- exception.
parseGenFileAsAlphabet :: [String] -> Int -> Either (Int, GFPError) [String]
parseGenFileAsAlphabet lines num = case (parseGenFileAsDict lines num) of
    Left err                        -> Left err
    Right (MonoidalGenSummary dict) -> Right (toAlphabet dict)
