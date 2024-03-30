-- | Implements a parser for generator files.

module Tietze.Parse.GeneratorFile
  ( GFPError
  , GenFileSummary (..)
  , display
  , parseGenFileAsDict
  , parseGenFileAsAlphabet
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import           Tietze.Common
import           Tietze.Either
import           Tietze.Generators.Semantics
import           Tietze.Parse.Internal.GeneratorFile
import           Tietze.Parse.Semantics

-----------------------------------------------------------------------------------------
-- * Generator File Parsing Errors.

instance Display GenFileError where
    display MissingSemModel         = "Semantics model not provided."
    display (UnknownSemModel model) = "Unknown semantic model (" ++ model ++ ")."
    display (InvalidSemArgs args)   = "Unknown semantic arguments (" ++ args ++ ")."
    display (SemModelWOImpl model)  = display model ++ " not implemented."
    display InvalidGenName          = "Generator name started with invalid symbol."
    display (InvalidGenSem pos msg) = "Invalid semv at " ++ show pos ++ " (" ++ msg ++ ")."
    display (DuplicateGenName name) = "Duplicate generator name (" ++ name ++ ")."

-----------------------------------------------------------------------------------------
-- * Full Generator File Parsing.

-- | Lifts semantic value types to the return value. This is a cleaner alternative to the
-- type: Either (GenDict SemV1) (Either (GenDict SemV2), (Either SemV3 (Either ...)))).
--
-- In the future, this might also carry on parameters that describe the generator file.
data GenFileSummary = MonoidGenSummary (GenDict ())
                    | DyadicTwoSummary (GenDict TwoQubitDyadic)
                    | DyadicThreeSummary (GenDict ThreeQubitDyadic)
                    | ModMultProductSummary (GenDict MultProductModP) [Int]
                    | ModAddProductSummary (GenDict AddProductModP) [Int]
                    deriving (Eq,Show)

-- | Consumes all lines of a generator file (lines). If the lines are valid, then returns
-- a dictionary of all generators and their semantics, wrapped by their semantic model.
-- Otherwise, returns a parsing exception.
parseGenFileAsDict :: [String] -> Int -> Either (Int, GFPError) GenFileSummary
parseGenFileAsDict lines num =
    branchRight (parseSemanticModel lines 1)
        (\(sem, semLn, gens) -> let nextLn = semLn + 1 in case sem of
            MonoidSem ->
                updateRight (parseGenDict parseMonoidSem gens nextLn)
                            MonoidGenSummary
            DyadicTwoSem ->
                updateRight (parseGenDict interpret2QubitCliffordDTofGate gens nextLn)
                            DyadicTwoSummary
            DyadicThreeSem ->
                updateRight (parseGenDict interpret3QubitCliffordDTofGate gens nextLn)
                            DyadicThreeSummary
            (MultModPSem pvals) ->
                updateRight (parseGenDict (interpretMultProductModP pvals) gens nextLn)
                            (`ModMultProductSummary` pvals)
            (AddModPSem pvals) ->
                updateRight (parseGenDict (interpretAddProductModP pvals) gens nextLn)
                            (`ModAddProductSummary` pvals)
            _ -> Left (semLn, Right (SemModelWOImpl sem)))

-- | A GenFileSummary carries the type data of the underlying semantic model. This
-- function allows all semantic data to be stripped away, returning instead a list of
-- generator symbols. Consumes all lines of a generator file (lines). If the lines are
-- valid, then returns a list of all generator symbols. Otherwise, returns a parsing
-- exception.
parseGenFileAsAlphabet :: [String] -> Int -> Either (Int, GFPError) [String]
parseGenFileAsAlphabet lines num =
    case parseGenFileAsDict lines num of
        Left err                             -> Left err
        Right (MonoidGenSummary dict)        -> impl dict
        Right (DyadicTwoSummary dict)        -> impl dict
        Right (DyadicThreeSummary dict)      -> impl dict
        Right (ModMultProductSummary dict _) -> impl dict
        Right (ModAddProductSummary dict _)  -> impl dict
    where impl dict = Right (toAlphabet dict)
