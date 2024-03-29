-- | Internals for RelationFiles. Enables unit testing.

module Tietze.Parse.Internal.RelationFile
  ( RelFileError (..)
  , RFPError
  , findUnknownGenInRule
  , parseRule
  , parseRuleDefn
  , updateRules
  , parseRelLine
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import           Tietze.Common
import           Tietze.Either
import           Tietze.Maybe
import           Tietze.Parse.Common
import           Tietze.Parse.MonWords
import           Tietze.Rewrite.Lookup
import           Tietze.Rewrite.Rules

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

-- | Errors returned during generator file parsing.
type RFPError = Either ParserError RelFileError

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
    where update = relToAbsErrPos str substr

-----------------------------------------------------------------------------------------
-- * Functions to Validate Monoidal Words.

-- | Consumes a list of generator names (gens) and a rule. Returns the first symbol (on
-- either the lhs, or rhs, with priority for the lhs) which violates the checks of
-- findUnknownGenInMonWord. If no such symbol exists, then nothing is returned.
findUnknownGenInRule :: [String] -> RewriteRule -> Maybe Symbol
findUnknownGenInRule gens rule = branchNothing (findUnknownGenInMonWord gens (lhs rule))
                                               (findUnknownGenInMonWord gens (rhs rule))

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

-- | Consumes an identifier, and a string that represents a rewrite rule (str). Attempts
-- to parse a rule of the form <MON_WORD> <OP> <MON_WORD> where <OP> is one of = or →.
-- The source of the rule is set to the identifier. Requires that str does not have
-- leading whitespace. Error messages are given with respect to indices in str.
parseRule :: String -> String -> Either RFPError RewriteRule
parseRule id str =
    case parseMonWord str of
        Just (lhs, opStr) -> case parseFromSeps ["→", "="] opStr of
            Just (op, rhsStr) -> case parseMonWord $ snd $ trimSpacing rhsStr of
                Just (rhs, post) -> let lval = Left $ UnexpectedSymbol $ getErrPos str post
                                        rval = RewriteRule lhs rhs (op == "=") (Primitive id)
                                    in branchOnSpacing post lval rval
                Nothing -> Left $ Right RuleMissingRHS
            Nothing -> Left $ Right $ InvalidRuleType $ getErrPos str opStr
        Nothing -> Left $ Right RuleMissingLHS

-- | Consumes a single line of a relation file (str). Attempts to parse a named rewrite
-- rule from str. If parsing is successful, then (id, rule) is returned where rule is a
-- rewrite rule and id is its name. Otherwise, an error type is returned.
parseRuleDefn :: String -> Either RFPError (String, RewriteRule)
parseRuleDefn str =
    case parseId $ snd $ trimSpacing str of
        Just (id, rstr) -> let (isTrimmed, trimmed) = trimSpacing rstr
                           in if isTrimmed
                              then case parseRule id trimmed of
                                    Left err   -> Left $ propRelErr str trimmed err
                                    Right rule -> Right (id, rule)
                              else Left $ Left $ UnexpectedSymbol $ getErrPos str rstr
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
    branchRight (parseRuleDefn str) $ \(id, rule) ->
        case findUnknownGenInRule gens rule of
            Just gen -> Left $ Right $ UnknownGenName $ display gen
            Nothing  -> if dict `hasRule` id
                        then Left $ Right $ DuplicateRuleName id
                        else Right $ dict `addRule` (id, rule)

-----------------------------------------------------------------------------------------
-- * File Parsing Methods.

-- | Consumes  a list of generators (gens), a partial rule dictionary (dict), a single
-- line of a relation file (line), and the current line number (num). If the line defines
-- a valid relation, then returns dict with the new relation inserted. If the line is
-- empty, then dict is return. Otherwise, returns a parsing exception.
parseRelLine :: [String] -> RuleDict -> String -> Int -> Either (Int, RFPError) RuleDict
parseRelLine gens dict line num
    | trimmed == "" = Right dict
    | otherwise     = updateLeft (updateRules dict gens trimmed)
                                 (\err -> (num, propRelErr stripped trimmed err))
    where stripped     = stripComments line
          (_, trimmed) = trimSpacing stripped
