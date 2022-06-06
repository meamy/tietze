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
