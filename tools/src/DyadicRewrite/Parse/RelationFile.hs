-- | Implements a parser for generator files.

module DyadicRewrite.Parse.RelationFile where

import DyadicRewrite.Rewrite.Rules
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
-- * Line Parsing Methods.

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
