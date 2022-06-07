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
                  | ApplyOnPRel
                  | RewriteOnDRel
                  | UnknownGenName String
                  | UnknownRelName String
                  deriving (Eq)

instance Show DerFileError where
    show UnknownRelMod         = "Unknown relation modifier (a symbol prefixed by !)."
    show InvalidRelName        = "Relation name started with invalid symbol."
    show InvalidAppPos         = "Expected position at end of rewrite operation."
    show InvalidAppDir         = "Non-equational relation applied right-to-left."
    show MissingAppDir         = "Equational relation requires application direction."
    show ApplyOnPRel           = "Applied use of a primitive relation."
    show RewriteOnDRel         = "Primitive use of a derived relation."
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
                                then parseAppPos rel isL2R str
                                else if isL2R
                                     then Left (Right MissingAppDir)
                                     else Left (Right InvalidAppDir)
    where isDirected    = (not (dir == ""))
          isL2R         = (not (dir == "←"))
          dirMatchesRel = if (equational rel) then isDirected else isL2R
