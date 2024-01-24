-- | Utilities to generate dependency graphs.

module LafontExe.Logic.GraphDeps where

import qualified Data.Map                   as Map
import           Lafont.Either
import           Lafont.Format.GraphViz
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Summary
import           LafontExe.IO.Configs

-----------------------------------------------------------------------------------------
-- * Annotated DotFile Generation.

-- | Associates the name of a derivation, represented as a NodeID, with the following
-- data in the following order:
-- 1. the type of the derivation as a string.
data NodeSummary = NodeSummary NodeID String

-- | Associates a DotFile with a list of NodeSummaries, such that each node in the graph
-- with a non-trivial NodeSummary has an entry in the list.
data AnnotatedDotFile = AnnotatedDotFile DotFile [NodeSummary]

-- | Returns the a list of non-trivial NodeSummaries from a list of derivations.
derivationsToDotSummary :: [AbsDerivation] -> [NodeSummary]
derivationsToDotSummary []                           = []
derivationsToDotSummary (AbsDerivation sum _ : list) =
    case propName $ meta sum of
        Nothing   -> dotsum
        Just name -> case propType $ meta sum of
            Nothing -> dotsum
            Just ty -> let nid = unsafeToNodeID name
                       in NodeSummary nid ty : dotsum
    where dotsum = derivationsToDotSummary list

-- | Takes as input a list of derivations (derivations). If derivations contains an
-- umet dependency, then an UnmetDep error is returned. Otherwise, the derivation graph
-- associated with derivations is generated. This graph is then converted into a DotFile
-- with default styling. Finally, the DotFile is annotated with the NodeSummaries for all
-- derivations in derivations (see derivationsToDotSummary).
derivationsToDotFile :: [AbsDerivation] -> Either UnmetDep AnnotatedDotFile
derivationsToDotFile derivations =
    branchRight (derivationToGraph derivations) $ \deps ->
        let g   = unwrapDepGraph deps
            dot = graphToDotFile g unsafeToNodeID
            sum = derivationsToDotSummary derivations
        in Right $ AnnotatedDotFile dot sum

-- | Returns the underlying DotFile from an AnnotatedDotFile.
unwrapDotFile :: AnnotatedDotFile -> DotFile
unwrapDotFile (AnnotatedDotFile dot _) = dot

-----------------------------------------------------------------------------------------
-- * Annotated DotFile Decoration.

-- | Implementation details for applyNodeColour. Requres the node summary be separated
-- from the DotFile in the AnnotatedDotFile. The unannotated DotFile is return.
applyNodeColourImpl :: ColorMap -> DotFile -> [NodeSummary] -> DotFile
applyNodeColourImpl _    dot []                          = dot
applyNodeColourImpl cmap dot (NodeSummary nid ty : list) =
    case Map.lookup ty cmap of
        Nothing -> dot'
        Just c  -> setNodeColour dot' nid c
    where dot' = applyNodeColourImpl cmap dot list

-- | Takes as input a color map (cmap) and an annotated DotFile. For each node nid in the
--  annotated DotFile, if nid is of type ty and type ty is associated with colour c in
-- cmap, then the colour of nid is set to c.
applyNodeColour :: ColorMap -> AnnotatedDotFile -> AnnotatedDotFile
applyNodeColour cmap (AnnotatedDotFile dot sum) = AnnotatedDotFile dot' sum
    where dot' = applyNodeColourImpl cmap dot sum
