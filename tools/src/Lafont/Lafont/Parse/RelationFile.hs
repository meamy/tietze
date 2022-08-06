-- | Implements a parser for generator files.

module Lafont.Parse.RelationFile where

import Lafont.Common
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Lookup
import Lafont.Parse.Common
import Lafont.Parse.MonWords

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

-- | Errors unique to generator file parsing.
data RelFileError = InvalidRuleName
                  | RuleMissingLHS 
                  | InvalidRuleType Int
                  | RuleMissingRHS
                  | UnknownGenName String
                  | DuplicateRuleName String
                  deriving (Eq,Show)

instance Display RelFileError where
    display InvalidRuleName          = "Rule name started with invalid symbol."
    display RuleMissingLHS           = "Rule missing left-hand side."
    display (InvalidRuleType pos)    = "Invalid rule type at pos " ++ show pos ++ "."
    display RuleMissingRHS           = "Rule missing right-hand side."
    display (UnknownGenName name)    = "Unknown generator name (" ++ name ++ ")."
    display (DuplicateRuleName name) = "Duplicate rule name (" ++ name ++ ")."

-- | Errors returned during generator file parsing.
type RFPError = Either ParserError RelFileError

-----------------------------------------------------------------------------------------
-- * Functions to Validate Monoidal Words.

-- | Consumes a list of generator names (gens) and a rule. Returns the first symbol (on
-- either the lhs, or rhs, with priority for the lhs) which violates the checks of
-- findUnknownGenInMonWord. If no such symbol exists, then nothing is returned.
findUnknownGenInRule :: [String] -> RewriteRule -> Maybe Symbol
findUnknownGenInRule gens rule =
    case findUnknownGenInMonWord gens (lhs rule) of
        Just gen -> Just gen
        Nothing  -> findUnknownGenInMonWord gens (rhs rule)

-----------------------------------------------------------------------------------------
-- * Line Parsing Helper Methods.

-- | Helper function to propogation relation file errors from unnamed rewrite rule
-- parsing to named rewrite rule parsing. For example, if an error occurs at index 5 of
-- substr, and if substr appears at index 7 of str, then the error is updated to display
-- index 12.
--
-- Note: All cases are stated explicitly, so that adding a new positional error without
-- updating this method will result in a compile-time type error.
propRelErr :: String -> String -> RFPError -> RFPError
propRelErr str substr (Left err)  = Left (propCommonErr str substr err)
propRelErr str substr (Right err) =
    case err of
        InvalidRuleName          -> Right InvalidRuleName
        RuleMissingLHS           -> Right RuleMissingLHS
        (InvalidRuleType pos)    -> Right (InvalidRuleType (update pos))
        RuleMissingRHS           -> Right RuleMissingRHS
        (UnknownGenName name)    -> Right (UnknownGenName name)
        (DuplicateRuleName name) -> Right (DuplicateRuleName name)
    where update pos = relToAbsErrPos str substr pos

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

-- | Consumes a string that represents a rewrite rule (str). Attempts to parse a rule of
-- the form <MON_WORD> <OP> <MON_WORD> where <OP> is one of = or →. Requires that str
-- does not have leading whitespace. Error messages are given with respect to indices in
-- str.
parseRule :: String -> Either RFPError RewriteRule
parseRule str =
    case parseMonWord str of
        Just (lhs, opStr) -> case parseFromSeps ["→", "="] opStr of
            Just (op, rhsStr) -> case parseMonWord $ snd $ trimSpacing rhsStr of
                Just (rhs, post) -> let lval = Left (UnexpectedSymbol (getErrPos str post))
                                        rval = RewriteRule lhs rhs (op == "=") Nothing
                                    in branchOnSpacing post lval rval
                Nothing -> Left (Right RuleMissingRHS)
            Nothing -> Left (Right (InvalidRuleType (getErrPos str opStr)))
        Nothing -> Left (Right RuleMissingLHS)

-- | Consumes a single line of a relation file (str). Attempts to parse a named rewrite
-- rule from str. If parsing is successful, then (id, rule) is returned where rule is a
-- rewrite rule and id is its name. Otherwise, an error type is returned.
parseRuleDefn :: String -> Either RFPError (String, RewriteRule)
parseRuleDefn str =
    case parseId $ snd $ trimSpacing str of
        Just (id, ruleStr) -> let (isTrimmed, trimmed) = trimSpacing ruleStr
                              in if isTrimmed
                                 then case parseRule trimmed of
                                          Left err   -> Left (propRelErr str trimmed err)
                                          Right rule -> Right (id, rule)
                                 else Left (Left (UnexpectedSymbol (getErrPos str ruleStr)))
        Nothing -> Left (Right InvalidRuleName)

-- | Consumes a partial map of rewrite rules (dict), a list of generator names (gens),
-- and a single line of a generator file (str). Attemps to call (parseRuleDefn str) and
-- then add the result to dict. If parseGenerator fails, then the error is forwarded. If
-- a rewrite rule contains an unknown generator, then an UnknownGeneratorName error is
-- returned. If the rewrite rule's name is a duplicate, then a DuplicateRuleName error is
-- returned. If no errors occur, then the ID and rewrite rule returned by parseGenerator
-- are added to dict. The resulting dict is returned.
updateRules :: RuleDict -> [String] -> String -> Either RFPError RuleDict
updateRules dict gens str =
    case parseRuleDefn str of
        Left err        -> Left err
        Right (id, rule) -> case findUnknownGenInRule gens rule of
            Just gen -> Left (Right (UnknownGenName (display gen)))
            Nothing  -> if dict `hasRule` id
                        then Left (Right (DuplicateRuleName id))
                        else Right (dict `addRule` (id, rule))

-----------------------------------------------------------------------------------------
-- * File Parsing Methods.

-- | Consumes all lines of a relation file (lines), a list of generator names (gens), and
-- the current line number (num). If the lines are valid, then returns a dictionary of
-- all rewrite rules. Otherwise, returns a parsing exception.
parseRelFile :: [String] -> [String] -> Int -> Either (Int, RFPError) RuleDict
parseRelFile _    []           _   = Right empty
parseRelFile gens (line:lines) num =
    case parseRelFile gens lines (num + 1) of
        Left  err  -> Left err
        Right dict -> if trimmed == ""
                      then Right dict
                      else case updateRules dict gens trimmed of
                                Left err   -> Left (num, propRelErr stripped trimmed err)
                                Right dict -> Right dict
    where stripped = stripComments line
          (_, trimmed) = trimSpacing stripped
