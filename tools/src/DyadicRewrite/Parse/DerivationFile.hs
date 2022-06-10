-- | Implements a parser for derivation files.

module DyadicRewrite.Parse.DerivationFile where

import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

-- |
data DerFileError = UnknownRewriteMod
                  | InvalidRuleName
                  | InvalidRewritePos
                  | InvalidRewriteDir
                  | MissingRewriteDir
                  | ApplyOnPrimitive
                  | RewriteOnDerived
                  | UnknownGenName String
                  | UnknownRuleName String
                  deriving (Eq)

instance Show DerFileError where
    show UnknownRewriteMod      = "Unknown rewrite modifier (a symbol prefixed by !)."
    show InvalidRuleName        = "Rewrite rule name starts with invalid symbol."
    show InvalidRewritePos      = "Expected position at end of rewrite."
    show InvalidRewriteDir      = "Non-equational rewrite rule applied right-to-left."
    show MissingRewriteDir      = "Equational rewrite rule requires derivation direction."
    show ApplyOnPrimitive       = "Applied use of a primitive rewrite rule."
    show RewriteOnDerived       = "Primitive use of a derived rewrite rule."
    show (UnknownRuleName name) = "Unknown rewrite rule (" ++ name ++ ")."

-- | Errors returned during derivation file parsing.
type DFPError = Either ParserError DerFileError

-----------------------------------------------------------------------------------------
-- * Line Parsing Helper Methods.

-- | Helper function to propogation derivation file errors from a callee parsing function
-- to a caller parsing function. For example, if an error occurs at index 5 of substr,
-- and if substr appears at index 7 of str, then the error is updated to index 12.
propDerErr :: String -> String -> DFPError -> DFPError
propDerErr str substr err =
    case err of
        Left (UnexpectedSymbol pos) -> Left (UnexpectedSymbol (update pos))
        otherwise                   -> err
    where update pos = relToAbsErrPos str substr pos

-- | Helper function to parse the position at the end of a rewrite. Assumes that a rule
-- and derivation direction (isLeftToRight) have already been parsed, str is the
-- remaining input, and that str has no leading spacing.
parseRewritePos :: RewriteRule -> Bool -> String -> Either DFPError Rewrite
parseRewritePos rule isLeftToRight str =
    case (parseNat str) of
        Just (pos, post) -> let lval = Left (UnexpectedSymbol (getErrPos str post))
                                rval = Rewrite rule pos isLeftToRight
                            in branchOnSpacing post lval rval
        Nothing -> Left (Right InvalidRewritePos)

-- | Consumes a rule a requested derivation rule derivation direction (dir),
-- and the remaining input to be parsed (str). If the requested dir aligns with rule,
-- then the rewrite position is parsed from str and the resulting rewrite (or error) is
-- returned. Otherwise, the misalignment between rule and dir is described through an
-- error value.
parseRewriteDirAndPos :: RewriteRule -> String -> String -> Either DFPError Rewrite
parseRewriteDirAndPos rule dir str = if dirMatchesRule
                                then parseRewritePos rule isL2R (snd (trimSpacing str))
                                else if isL2R
                                     then Left (Right MissingRewriteDir)
                                     else Left (Right InvalidRewriteDir)
    where isDirected    = (not (dir == ""))
          isL2R         = (not (dir == "←"))
          dirMatchesRule = if (equational rule) then isDirected else isL2R

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

-- | Consumes a dictionary of known rules (dict) and an input string (str). Attempts to
-- parse a primitive rewrite of either the form <ID> <DIR> <POS> or <ID> <POS>. If
-- parsing is successful, then the corresponding rewrite is returned. Otherwise, an error
-- is returned.
parseRewrite :: RuleDict -> String -> Either DFPError Rewrite
parseRewrite dict str =
    case (parseId str) of
        Just (id, detStr) -> case (interpretRule dict id) of
            Just rule -> case (parseFromSeps ["→", "←", ""] detStr) of
                Just (dir, natStr) -> case (parseRewriteDirAndPos rule dir natStr) of
                    Left err -> Left (propDerErr str natStr err)
                    Right rw -> Right rw
                Nothing ->  Left (Left UnknownParseError) -- Should be unreachable.
            Nothing -> Left (Right (UnknownRuleName id))
        Nothing -> Left (Right InvalidRuleName)

-- | Consumes a dictionary of known rules (dict) and a rewrite line of a derivation file
-- (str). Attempts to parse str, taking into account all modifiers applied to the line.
-- If parsing is successful, then the corresponding rewrite is returned. Otherwise, an
-- error is returned. 
parseRewriteLine :: RuleDict -> String -> Either DFPError Rewrite
parseRewriteLine dict str =
    case (parseFromSeps ["!apply", "!"] str) of
        Just ("!apply", rwStr) -> let trimmed = (snd (trimSpacing rwStr))
                                  in case (parseRewrite dict trimmed) of
                                         Left err -> Left (propDerErr str trimmed err)
                                         Right rw -> if (derived (rule rw))
                                                     then Right rw
                                                     else Left (Right ApplyOnPrimitive)
        Nothing -> case (parseRewrite dict str) of
            Left err -> Left err
            Right rw -> if (derived (rule rw))
                        then Left (Right RewriteOnDerived)
                        else Right rw
        Just ("!", _) -> Left (Right UnknownRewriteMod)
