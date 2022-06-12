-- | Implements a parser for derivation files.

module DyadicRewrite.Parse.DerivationFile where

import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.MonWords
import DyadicRewrite.Parse.Properties

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
--
-- Note: All cases are stated explicitly, so that adding a new positional error without
-- updating this method will result in a compile-time type error.
propDerErr :: String -> String -> DFPError -> DFPError
propDerErr str substr (Left err)  = Left (propCommonErr str substr err)
propDerErr str substr (Right err) =
    case err of
        UnknownRewriteMod      -> Right UnknownRewriteMod
        InvalidRuleName        -> Right InvalidRuleName
        InvalidRewritePos      -> Right InvalidRewritePos
        InvalidRewriteDir      -> Right InvalidRewriteDir
        MissingRewriteDir      -> Right MissingRewriteDir
        ApplyOnPrimitive       -> Right ApplyOnPrimitive
        RewriteOnDerived       -> Right RewriteOnDerived
        (UnknownRuleName name) -> Right (UnknownRuleName name)
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

-----------------------------------------------------------------------------------------
-- * Preamble Parsing.

-- | Maintains metadata about a derivation file.
data RewritePreamble = RewritePreamble { propName :: Maybe String
                                       } deriving (Show,Eq)

-- | Creates a RewritePreamble will all metadata set to N/A.
defaultPreamble :: RewritePreamble
defaultPreamble = RewritePreamble Nothing

-- | Consumes a derivation name (str) and a rewrite preamble. If the preamble does not
-- have a name, then a new preamble is returned with the name field set to str and all
-- other fields unchanged. Otherwise, nothing is returned.
setName :: PropSetter String RewritePreamble
setName str (RewritePreamble Nothing)  = Just (RewritePreamble (Just str))
setName _   (RewritePreamble (Just _)) = Nothing

-- | A dictionary of all preamble properties.
rewriteProperties :: PropertyDict RewritePreamble
rewriteProperties = noProps `addProp` (makePropPair "name" parseId setName)

-- | A parser for rewriteProperties.
parseRewritePreamble :: PropParser RewritePreamble
parseRewritePreamble = makePreambleParser rewriteProperties defaultPreamble

-----------------------------------------------------------------------------------------
-- * Full Derivation File Parsing.

-- | Consumes the body of a derivation file (excluding the initial word). Attempts to
-- find the final word in the file. If the final word is found, then the word is returned
-- along with all lines which follow the word. Otherwise, nothing is returned.
parseFinalMonWord :: [String] -> Maybe ([String], MonWord)
parseFinalMonWord []           = Nothing
parseFinalMonWord (line:lines) =
    case (parseFinalMonWord lines) of
        Nothing -> case (parseLineAsMonWord line) of
            Just word -> Just ([], word)
            Nothing   -> Nothing
        Just (body, word) -> Just ((line:body), word)
