-- | Implements a parser for generator files.

module Lafont.Parse.RelationFile (
    -- Re-exports from internal.
    RelFileError,
    RFPError,
    -- Exports.
    display,
    findUnknownGenInRule,
    parseRelFile
) where

import           Lafont.Common
import           Lafont.Either
import           Lafont.Parse.Internal.RelationFile
import           Lafont.Rewrite.Lookup

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

instance Display RelFileError where
    display InvalidRuleName          = "Rule name started with invalid symbol."
    display RuleMissingLHS           = "Rule missing left-hand side."
    display (InvalidRuleType pos)    = "Invalid rule type at pos " ++ show pos ++ "."
    display RuleMissingRHS           = "Rule missing right-hand side."
    display (UnknownGenName name)    = "Unknown generator name (" ++ name ++ ")."
    display (DuplicateRuleName name) = "Duplicate rule name (" ++ name ++ ")."

-----------------------------------------------------------------------------------------
-- * File Parsing Methods.

-- | Consumes all lines of a relation file (lines), a list of generator names (gens), and
-- the current line number (num). If the lines are valid, then returns a dictionary of
-- all rewrite rules. Otherwise, returns a parsing exception.
parseRelFile :: RuleDict -> [String] -> [String] -> Int -> Either (Int, RFPError) RuleDict
parseRelFile last _    []           _   = Right last
parseRelFile last gens (line:lines) num =
    branchRight (parseRelFile last gens lines (num + 1))
                (\dict -> parseRelLine gens dict line num)
