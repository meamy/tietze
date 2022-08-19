-- | Implements a parser for derivation files.

module Lafont.Parse.DerivationFile where

import           Lafont.Common
import           Lafont.Maybe
import           Lafont.Parse.Common
import           Lafont.Parse.MonWords
import           Lafont.Parse.Properties
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

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
-- and derivation direction (dir) have already been parsed, str is the remaining input,
-- and that str has no leading spacing.
parseRewriteAtPos :: Bool -> String -> Either DFPError (RulePos, RuleDir)
parseRewriteAtPos isLeftToRight str =
    case parseNat str of
        Just (pos, post) -> let lval = Left (UnexpectedSymbol (getErrPos str post))
                                rval = (pos, dir)
                            in branchOnSpacing post lval rval
        Nothing -> Left (Right InvalidRewritePos)
    where dir = if isLeftToRight then L2R else R2L

-- | Consumes whether a rule is equational, a requested derivation rule derivation
-- direction (dir),  and the remaining input to be parsed (str). If the requested dir
-- aligns with rule, then the rewrite position is parsed from str and the resulting
-- rewrite description (or error) is returned. Otherwise, the misalignment between rule
-- and dir is described through an error value.
parseRewriteAtDirAndPos :: Bool -> String -> String -> Either DFPError (RulePos, RuleDir)
parseRewriteAtDirAndPos isEqn dir str
    | dirMatchesRule = parseRewriteAtPos isL2R (snd (trimSpacing str))
    | isL2R          = Left (Right MissingRewriteDir)
    | otherwise      = Left (Right InvalidRewriteDir)
    where isDirected     = dir /= ""
          isL2R          = dir /= "←"
          dirMatchesRule = if isEqn then isDirected else isL2R

-----------------------------------------------------------------------------------------
-- * Line Parsing Methods.

-- | Consumes the textual representation of a rewrite rule (str). Attempts to tokenize
-- the string into <ID> <DIR> <POS>. If tokenizing is successful, then the three strings
-- are returned. Otherwise, the corresponding error is returned.
splitRewrite :: String -> Either DFPError (String, String, String)
splitRewrite str =
    case parseId str of
        Just (id, details) -> case parseFromSeps ["→", "←", ""] details of
            Just (dir, pos) -> Right (id, dir, pos)
            Nothing         -> Left (Left UnknownParseError) -- Should be unreachable.
        Nothing -> Left (Right InvalidRuleName)

-- | Consumes a dictionary of known rules (dict) and an input string (str). Attempts to
-- parse a primitive rewrite of either the form <ID> <DIR> <POS> or <ID> <POS>. If
-- parsing is successful, then the corresponding rewrite is returned. Otherwise, an error
-- is returned.
parseRewrite :: RuleDict -> String -> Either DFPError Rewrite
parseRewrite dict str =
    case splitRewrite str of
        Right (id, dirStr, posStr) -> case interpretRule dict id of
            Just rule -> case parseRewriteAtDirAndPos (equational rule) dirStr posStr of
                Left err         -> Left (propDerErr str posStr err)
                Right (pos, dir) -> Right (Rewrite rule pos dir)
            Nothing -> Left (Right (UnknownRuleName id))
        Left err -> Left err

-- | Consumes a dictionary of known rules (dict), an apply line of a derivation file
-- (str), and the substring of str containing the modified rule line (rwStr). If parsing
-- is successful, then the corresponding rewrite is returned. Otherwise, an error is
-- returned.
parseApplyLine :: RuleDict -> String -> String -> Either DFPError Apply
parseApplyLine dict str rwStr =
    case parseRewrite dict trimmed of
        Left err                     -> Left (propDerErr str trimmed err)
        Right (Rewrite rule pos dir) -> case derivedFrom rule of
            Nothing -> Left (Right ApplyOnPrimitive)
            Just id -> Right (Apply id pos dir)
    where (_, trimmed) = trimSpacing rwStr

-- | Consumes a dictionary of known rules (dict) and a rule line of a derivation file
-- (str). Attempts to parse str. If parsing is successful, then the corresponding rewrite
-- is returned. Otherwise, an error is returned.
parseRuleLine :: RuleDict -> String -> Either DFPError Rewrite
parseRuleLine dict str =
    case parseRewrite dict str of
        Left err -> Left err
        Right rw -> let (Rewrite rule _ _) = rw
                    in if isDerivedRule rule
                       then Left (Right RewriteOnDerived)
                       else Right rw

-- | Consumes a dictionary of known rules (dict) and a rewrite line of a derivation file
-- (str). Attempts to parse str, taking into account all modifiers applied to the line.
-- If parsing is successful, then the corresponding rewrite is returned. Otherwise, an
-- error is returned.
parseRewriteLine :: RuleDict -> String -> Either DFPError AbsRewrite
parseRewriteLine dict str =
    case parseFromSeps ["!apply", "!"] str of
        Just ("!apply", rwStr) -> case parseApplyLine dict str rwStr of
            Left err -> Left err
            Right ap -> Right (Right ap)
        Nothing -> case parseRuleLine dict str of
            Left err -> Left err
            Right rw -> Right (Left rw)
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
rewriteProperties = noProps `addProp` makePropPair "name" parseId setName

-- | A parser for rewriteProperties.
parseRewritePreamble :: PropParser RewritePreamble
parseRewritePreamble = makePreambleParser rewriteProperties defaultPreamble

-----------------------------------------------------------------------------------------
-- * Derivation Body Parsing.

-- | Factors out return value structure of a derivation file parser.
type DParseRV a = Either (Int, DFPError) a

-- | The skeleton of a derivation, consisting of an initial word subsection, a rewrite
-- subsection, and a final word subsection
type DSkel = (MonWord, [String], MonWord)

-- | The summary of a derivation file (parsed) together with an unparsed rewrite section
-- (unparsed) starting at the specified line number (linenum).
data PreDerivation = ParDerivation { parsed   :: DerivationSummary
                                   , unparsed :: [String]
                                   , linenum  :: Int
                                   } deriving (Eq,Show)

-- | Consumes the body of a derivation file (excluding the initial word). Attempts to
-- find the final word in the file. If the final word is found, then the word is returned
-- along with all lines which follow the word. Otherwise, nothing is returned.
parseFinalMonWord :: [String] -> Maybe ([String], MonWord, [String])
parseFinalMonWord []           = Nothing
parseFinalMonWord (line:lines) =
    case parseLineAsMonWord line of
        Just word -> Just ([], word, lines)
        Nothing   -> maybeApply f $ parseFinalMonWord lines
    where f (body, word, rest) = (line : body, word, rest)

-- | Consumes a dictionary of known rules (dict) and the rewrite lines of a derivation
-- file. If the lines are valid with respect to dict, then returns a list of rewrites in
-- the order they appear. Otherwise, returns a parsing exception.
parseRewriteLines :: RuleDict -> [String] -> Int -> DParseRV [AbsRewrite]
parseRewriteLines _     []           _   = Right []
parseRewriteLines rules (line:lines) num
    | trimmed == "" = parseRewriteLines rules lines nextNum
    | otherwise     = case parseRewriteLine rules trimmed of
        Left err      -> Left (num, propDerErr stripped trimmed err)
        Right rewrite -> case parseRewriteLines rules lines nextNum of
            Left (errLn, err) -> Left (errLn, propDerErr stripped trimmed err)
            Right rewrites    -> Right (rewrite : rewrites)
    where stripped = stripComments line
          (_, trimmed) = trimSpacing stripped
          nextNum = num + 1

-- | Consumes the lines of a derivation file immediately following its preamble. If the
-- lines include an initial word subsection, a rewrite subsection, and a final word
-- subsection,  then a skeleton of the derivation section is returned. Otherwise, returns
-- a parsing exception. Requires that there is at least one line in the body, and that
-- the first line is the initial word.
--
-- Note: The initial and final words may be invalid. This is checked by preparseSection.
preparseSectionSkeleton :: [String] -> Int -> DParseRV (DSkel, [String])
preparseSectionSkeleton []              num = Left (num, Left UnexpectedEOF)
preparseSectionSkeleton (initLine:body) num =
    case parseLineAsMonWord initLine of
        Nothing   -> Left (num, Right MissingInitialWord)
        Just init -> case parseFinalMonWord body of
            Nothing                   -> Left (eofNum, Right MissingFinalWord)
            Just (steps, final, rest) -> Right ((init, steps, final), rest)
    where eofNum = num + length body + 1

-- | Consumes a list of known generator names (gens), the preamble of a derivation file
-- (meta), and a skeleton of the derivation. If the initial word subsection and the final
-- word subsection are valid, then a derivation summary is returned. Otherwise, returns a
-- parsing exception.
preparseSection :: [String] -> RewritePreamble -> DSkel -> Int -> DParseRV PreDerivation
preparseSection gens meta (init, steps, final) num =
    case findUnknownGenInMonWord gens init of
        Just gen -> Left (num, Right (UnknownGenName (name gen)))
        Nothing  -> case findUnknownGenInMonWord gens final of
            Just gen -> Left (finalNum, Right (UnknownGenName (name gen)))
            Nothing  -> let summary = DerivationSummary meta init final
                        in Right (ParDerivation summary steps stepsNum)
    where stepsNum = num + 1
          finalNum = stepsNum + length steps

-- |
preparseDerivation :: [String] -> [String] -> Int -> DParseRV (PreDerivation, [String])
preparseDerivation gens lines num =
    case parseRewritePreamble lines num of
        Left (errLn, err)          -> Left (errLn, Left err)
        Right (body, bodyLn, meta) -> case preparseSectionSkeleton body bodyLn of
            Left err           -> Left err
            Right (skel, rest) -> case preparseSection gens meta skel bodyLn of
                Left err  -> Left err
                Right pre -> Right (pre, rest)

-- | Consumes a list of known generator names (gens) and the lines of a derivation file
-- including the preamble (lines). If the lines include a preamble section, an initial
-- word section, a rewrite section, and a word string section, with the preamble,
-- initial, and final words valid, then a list of summaries for all derivations in the
-- file are returned. Otherwise, returns a parsing exception.
preparseDerivationFile :: [String] -> [String] -> Int -> DParseRV [PreDerivation]
preparseDerivationFile gens lines num =
    case preparseDerivation gens lines num of
        Left err          -> Left err
        Right (pre, rest) -> if isEOFSpacing rest
                             then Right [pre]
                             else let nextNum = num + length lines - length rest
                                  in case preparseDerivationFile gens rest nextNum of
                                      Left err  -> Left err
                                      Right res -> Right (pre : res)

-- | Consumes a dictionary of known rules, including derived rules (dict) and the summary
-- of a derivation file. If the body is a valid rewrite section with respect to rules,
-- then a derivation is returned. Otherwise, returns a parsing exception.
parseDerivationFile :: RuleDict -> PreDerivation -> DParseRV AbsDerivation
parseDerivationFile rules pre =
    case parseRewriteLines rules (unparsed pre) (linenum pre) of
        Left  err      -> Left err
        Right rewrites -> Right (AbsDerivation (parsed pre) rewrites)
