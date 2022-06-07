-- | Implements a parser for derivation files.

module DyadicRewrite.Parse.DerivationFile where

import Data.Either
import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

data DerFileError = UnknownRelMod
                  | InvalidRelName
                  | InvalidAppPos
                  | InvalidAppDir
                  | MissingAppDir
                  | ApplyOnPrim
                  | RewriteOnDeriv
                  | UnknownGenName String
                  | UnknownRelName String
                  deriving (Eq)

instance Show DerFileError where
    show UnknownRelMod         = "Unknown relation modifier (a symbol prefixed by !)."
    show InvalidRelName        = "Relation name started with invalid symbol."
    show InvalidAppPos         = "Expected position at end of rewrite operation."
    show InvalidAppDir         = "Non-equational relation applied right-to-left."
    show MissingAppDir         = "Equational relation requires application direction."
    show ApplyOnPrim           = "Applied use of a primitive relation."
    show RewriteOnDeriv        = "Primitive use of a derived relation."
    show (UnknownRelName name) = "Unknown relation name (" ++ name ++ ")."

-- | Errors returned during derivation file parsing.
type DFPError = Either ParserError DerFileError

-----------------------------------------------------------------------------------------
-- * Line parsing helper methods.

-- | Helper function to propogation derivation file errors from a callee parsing function
-- to a caller parsing function. For example, if an error occurs at index 5 of substr,
-- and if substr appears at index 7 of str, then the error is updated to index 12.
propDerErr :: String -> String -> DFPError -> DFPError
propDerErr str substr err =
    case err of
        Left (UnexpectedSymbol pos) -> Left (UnexpectedSymbol (update pos))
        otherwise                   -> err
    where update pos = relToAbsErrPos str substr pos

-- | Helper function to parse the position at the end of a derivation rule application.
-- Assumes that a relation (rel) and direction of applicaation (isLeftToRight) have
-- already been parsed, str is the remaining input, and that str has no leading spacing.
parseAppPos :: RewriteRule -> Bool -> String -> Either DFPError RewriteOp
parseAppPos rel isLeftToRight str =
    case (parseNat str) of
        Just (pos, post) -> let lval = Left (UnexpectedSymbol (getErrPos str post))
                                rval = RewriteOp rel pos isLeftToRight
                            in branchOnSpacing post lval rval
        Nothing -> Left (Right InvalidAppPos)

-- | Consumes a relation (rel), a requested derivation rule application direction (dir),
-- and the remaining input to be parsed (str). If the requested dir aligns with rel, then
-- the application position is parsed from str and the resulting operation (or error) is
-- returned. Otherwise, the misalignment between rel and dir is described through an
-- error value.
parseAppDirAndPos :: RewriteRule -> String -> String -> Either DFPError RewriteOp
parseAppDirAndPos rel dir str = if dirMatchesRel
                                then parseAppPos rel isL2R (snd (trimSpacing str))
                                else if isL2R
                                     then Left (Right MissingAppDir)
                                     else Left (Right InvalidAppDir)
    where isDirected    = (not (dir == ""))
          isL2R         = (not (dir == "←"))
          dirMatchesRel = if (equational rel) then isDirected else isL2R

-----------------------------------------------------------------------------------------
-- * Line parsing methods.

-- | Consumes a dictionary of known relations (rel) and an input string (str). Attempts
-- to parse a primitive rule operation of either the form <ID> <DIR> <POS> or <ID> <POS>.
-- If parsing is successful, then the corresponding RewriteOp is returned. Otherwise, an
-- error is returned.
parseApp :: RelDict -> String -> Either DFPError RewriteOp
parseApp dict str =
    case (parseId str) of
        Just (id, detStr) -> case (interpretRel dict id) of
            Just rel -> case (parseFromSeps ["→", "←", ""] detStr) of
                Just (dir, natStr) -> case (parseAppDirAndPos rel dir natStr) of
                    Left err -> Left (propDerErr str natStr err)
                    Right op -> Right op
                Nothing ->  Left (Left UnknownParseError) -- Should be unreachable.
            Nothing -> Left (Right (UnknownRelName id))
        Nothing -> Left (Right InvalidRelName)

-- | Consumes a dictionary of known relations (rel) and a rule application line of a
-- derivation file (str). Attempts to parse str, taking into account all modifiers
-- applied to the line. If parsing is successful, then the corresponding RewriteOp is
-- returned. Otherwise, an error is returned. 
parseRelApp :: RelDict -> String -> Either DFPError RewriteOp
parseRelApp dict str =
    case (parseFromSeps ["!apply", "!"] str) of
        Just ("!apply", opStr) -> let trimmed = (snd (trimSpacing opStr))
                                  in case (parseApp dict trimmed) of
                                         Left err -> Left (propDerErr str trimmed err)
                                         Right op -> if (derived (rule op))
                                                     then Right op
                                                     else Left (Right ApplyOnPrim)
        Nothing -> case (parseApp dict str) of
            Left err -> Left err
            Right op -> if (derived (rule op))
                        then Left (Right RewriteOnDeriv)
                        else Right op
        Just ("!", _) -> Left (Right UnknownRelMod)
