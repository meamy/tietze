-- | Implements a parser for derivation files.

module Lafont.Parse.DerivationFile where

import Lafont.Common
import Lafont.Maybe
import Lafont.Rewrite.Derivations
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Summary
import Lafont.Parse.Common
import Lafont.Parse.MonWords
import Lafont.Parse.Properties

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
                  | MissingInitialWord
                  | MissingFinalWord
                  deriving (Eq,Show)

instance Display DerFileError where
    display UnknownRewriteMod      = "Unknown rewrite modifier (a symbol prefixed by !)."
    display InvalidRuleName        = "Rewrite rule name starts with invalid symbol."
    display InvalidRewritePos      = "Expected position at end of rewrite."
    display InvalidRewriteDir      = "Non-equational rewrite rule applied right-to-left."
    display MissingRewriteDir      = "Equational rewrite rule requires derivation direction."
    display ApplyOnPrimitive       = "Applied use of a primitive rewrite rule."
    display RewriteOnDerived       = "Primitive use of a derived rewrite rule."
    display (UnknownGenName name)  = "Unknown generator name (" ++ name ++ ")."
    display (UnknownRuleName name) = "Unknown rewrite rule (" ++ name ++ ")."
    display MissingInitialWord     = "Initial word missing in derivation."
    display MissingFinalWord       = "Final word missing in derivation."

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
        (UnknownGenName name)  -> Right (UnknownGenName name)
        (UnknownRuleName name) -> Right (UnknownRuleName name)
        MissingInitialWord     -> Right MissingInitialWord
        MissingFinalWord       -> Right MissingFinalWord
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
parseRewriteDirAndPos rule dir str =
    if dirMatchesRule
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
                                         Right rw -> if (isDerivedRule (rule rw))
                                                     then Right rw
                                                     else Left (Right ApplyOnPrimitive)
        Nothing -> case (parseRewrite dict str) of
            Left err -> Left err
            Right rw -> if (isDerivedRule (rule rw))
                        then Left (Right RewriteOnDerived)
                        else Right rw
        Just ("!", _) -> Left (Right UnknownRewriteMod)

-----------------------------------------------------------------------------------------
-- * Preamble Parsing.

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
-- * Derivation Body Parsing.

-- | Factors out return value structure of a derivation file parser.
type DParseRV a = Either (Int, DFPError) a

-- | The summary of a derivation file (parsed) together with an unparsed rewrite section
-- (unparsed) starting at the specified line number (linenum).
data PreDerivation = ParDerivation { parsed :: DerivationSummary
                                   , unparsed :: [String]
                                   , linenum :: Int
                                   } deriving (Eq,Show)

-- | Consumes the body of a derivation file (excluding the initial word). Attempts to
-- find the final word in the file. If the final word is found, then the word is returned
-- along with all lines which follow the word. Otherwise, nothing is returned.
parseFinalMonWord :: [String] -> Maybe ([String], MonWord)
parseFinalMonWord []           = Nothing
parseFinalMonWord (line:lines) =
    case (parseFinalMonWord lines) of
        Just (body, word) -> Just ((line:body), word)
        Nothing           -> maybeApply (\word -> ([], word)) (parseLineAsMonWord line)

-- | Consumes a dictionary of known rules (dict) and the rewrite lines of a derivation
-- file. If the lines are valid with respect to dict, then returns a list of rewrites in
-- the order they appear. Otherwise, returns a parsing exception.
parseRewriteLines :: RuleDict -> [String] -> Int -> DParseRV [Rewrite]
parseRewriteLines _     []           _   = Right []
parseRewriteLines rules (line:lines) num =
    case (snd (trimSpacing stripped)) of
        ""   -> parseRewriteLines rules lines nextNum
        text -> case (parseRewriteLine rules text) of
            Left err      -> Left (num, (propDerErr stripped text err))
            Right rewrite -> case (parseRewriteLines rules lines nextNum) of
                Left (errLn, err) -> Left (errLn, (propDerErr stripped text err))
                Right rewrites    -> Right (rewrite:rewrites)
    where stripped = stripComments line
          nextNum = num + 1

-- | Consumes a list of known generator names (gens), the preamble of a derivation file
-- (meta), and the lines of a derivation file immediately following its preamble.  If the
-- lines include an inital word section, a rewrite section, and a final word section,
-- with both the initial and final words valid, then a derivation summary is returned.
-- Otherwise, returns a parsing exception. Requires that there is at least one line in
-- the body, and that the first line is the initial word.
preparseBody :: [String] -> RewritePreamble -> [String] -> Int -> DParseRV PreDerivation
preparseBody _    _    []                 num = Left (num, Left UnexpectedEOF)
preparseBody gens meta (initLine:body) num =
    case (parseLineAsMonWord initLine) of
        Nothing   -> Left (num, Right MissingInitialWord)
        Just init -> case (findUnknownGenInMonWord gens init) of
            Just gen -> Left (num, Right (UnknownGenName (name gen)))
            Nothing  -> case (parseFinalMonWord body) of
                Nothing            -> Left ((finAt body), Right MissingFinalWord)
                Just (rlines, fin) -> case (findUnknownGenInMonWord gens fin) of
                    Just gen -> Left ((finAt rlines), Right (UnknownGenName (name gen)))
                    Nothing  -> let sum = DerivationSummary meta init fin
                                in Right (ParDerivation sum rlines rln)
    where rln = num + 1
          finAt body = rln + (length body)

-- | Consumes a list of known generator names (gens) and the lines of a derivation file
-- including the preamble (lines). If the lines include a preamble section, an initial
-- word section, a rewrite section, and a word string section, with the preamble,
-- initial, and final words valid, then a then a derivation summary is returned.
-- Otherwise, returns a parsing exception
preparseDerivationFile :: [String] -> [String] -> Int -> DParseRV PreDerivation
preparseDerivationFile gens lines num =
    case (parseRewritePreamble lines num) of
        Left (errLn, err)          -> Left (errLn, Left err)
        Right (body, bodyLn, meta) -> preparseBody gens meta body bodyLn

-- | Consumes a dictionary of known rules, including derived rules (dict) and the summary
-- of a derivation file. If the body is a valid rewrite section with respect to rules,
-- then a derivation is returned. Otherwise, returns a parsing exception.
parseDerivationFile :: RuleDict -> PreDerivation -> DParseRV Derivation
parseDerivationFile rules pre =
    case (parseRewriteLines rules (unparsed pre) (linenum pre)) of
        Left  err      -> Left err
        Right rewrites -> Right (Derivation (parsed pre) rewrites)
