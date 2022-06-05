-- | Implements a parser for generator files.

module DyadicRewrite.Parse.RelationFile where

import Data.Maybe
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.Circuits

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

-- | Errors unique to generator file parsing.
data RelFileError = InvalidRelName
                  | RelMissingLHS 
                  | InvalidRelType Int
                  | RelMissingRHS
                  | UnknownGenName String
                  | DuplicateRelName String
                  deriving (Eq)

instance Show RelFileError where
    show InvalidRelName          = "Relation name started with invalid symbol."
    show RelMissingLHS           = "Relation missing left-hand side."
    show (InvalidRelType pos)    = "Invalid relation type as pos " ++ (show pos) ++ "."
    show RelMissingRHS           = "Relation missing right-hand side."
    show (UnknownGenName name)   = "Unknown generator name (" ++ name ++ ")."
    show (DuplicateRelName name) = "Duplicate relation name (" ++ name ++ ")."

-- | Errors returned during generator file parsing.
type RFPError = Either ParserError RelFileError

-----------------------------------------------------------------------------------------
-- * Helper functions to identify bad symbols in circuits.

-- | Consumes a list of generator names (gens) and a circuit (circ). Returns the first
-- gate in circuit with either a non-zero number of parameters or a name not in gens. If
-- no such gate exists, then nothing is returned.
findUnknownGenInCircuit :: [String] -> Circuit -> Maybe Gate
findUnknownGenInCircuit gens []          = Nothing
findUnknownGenInCircuit gens (gate:circ) = if gateIsValid
                                           then findUnknownGenInCircuit gens circ
                                           else Just gate
    where gateIsValid = ((params gate) == []) && ((name gate) `elem` gens)

-- | Consumes a list of generator names (gens) and a relation (rel). Returns the first
-- gate (on either the lhs, or rhs, with priority for the lhs) which violates the checks
-- of findUnknownGenInCircuit. If no such gate exists, then nothing is returned.
findUnknownGenInRel :: [String] -> RewriteRule -> Maybe Gate
findUnknownGenInRel gens rule =
    case (findUnknownGenInCircuit gens (lhs rule)) of
        Just gen -> Just gen
        Nothing  -> (findUnknownGenInCircuit gens (rhs rule))

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

-- | Helper function to propogation relation file errors from unnamed relation parsing to
-- named relation parsing. For example, if an error occurs at index 5 of substr, and if
-- substr appears at index 7 of str, then the error is updated to index 12.
propRelErr :: String -> String -> RFPError -> RFPError
propRelErr str substr err =
    case err of
        Left (UnexpectedSymbol pos) -> Left (UnexpectedSymbol (update pos))
        Right (InvalidRelType pos)  -> Right (InvalidRelType (update pos))
        otherwise                   -> err
    where update pos = relToAbsErrPos str substr pos

-- | Consumes a string that represents a rewrite relation (str). Attempts to parse a
-- relation of the form <CIRCUIT> <OP> <CIRCUIT> where <OP> is one of = or →. Requires
-- that str does not have leading whitespace. Error messages are given with respect to
-- indices in str.
parseRelation :: String -> Either RFPError RewriteRule
parseRelation str =
    case (parseCircuit str) of
        Just (lhs, opStr) -> case (parseFromSeps ["→", "="] opStr) of
            Just (op, rhsStr) -> case (parseCircuit (snd (trimSpacing rhsStr))) of
                Just (rhs, post) -> let lval = Left (UnexpectedSymbol (getErrPos str post))
                                        rval = RewriteRule lhs rhs (op == "=") False
                                    in branchOnSpacing post lval rval
                Nothing -> Left (Right RelMissingRHS)
            Nothing -> Left (Right (InvalidRelType (getErrPos str opStr)))
        Nothing -> Left (Right RelMissingLHS)

-- | Consumes a single line of a relation file (str). Attempts to parse a named relation
-- from str. If parsing is successful, then (id, rel) is returned where rel is a rewrite
-- rule and id is its name. Otherwise, an error type is returned.
parseRelationDefn :: String -> Either RFPError (String, RewriteRule)
parseRelationDefn str =
    case (parseId (snd (trimSpacing str))) of
        Just (id, relStr) -> let (isTrimmed, trimmed) = trimSpacing relStr
                             in if isTrimmed
                                then case (parseRelation trimmed) of
                                         Left err   -> Left (propRelErr str trimmed err)
                                         Right rule -> Right (id, rule)
                                else Left (Left (UnexpectedSymbol (getErrPos str relStr)))
        Nothing -> Left (Right InvalidRelName)

-- | Consumes a partial map of relations (dict), a list of generator names (gen)s, and a
-- single line of a generator file (str). Attemps to call (parseRelationDefn str) and
-- then add the result to dict. If parseGenerator fails, then the error is forwarded. If
-- a relation contains an unknown generator, then an UnknownGeneratorName error is
-- returned. If the relation name is a duplicate, then a DuplicateRelName error is
-- returned. If no errors occur, then the ID and relation returned by parseGenerator are
-- added to dict. The resulting dict is returned.
updateRelations :: RelDict -> [String] -> String -> Either RFPError RelDict
updateRelations dict gens str =
    case (parseRelationDefn str) of
        Left err        -> Left err
        Right (id, rel) -> case (findUnknownGenInRel gens rel) of
            Just gen -> Left (Right (UnknownGenName (show gen)))
            Nothing  -> if (dict `hasRel` id)
                        then Left (Right (DuplicateRelName id))
                        else Right (dict `addRel` (id, rel))
