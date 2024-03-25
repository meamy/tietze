-- | Utilities to generate dependency graphs.

module TietzeExe.Logic.GraphDeps
  ( AnnotatedDotFile
  , applyNodeColour
  , derivationsToDotFile
  , unwrapDotFile
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map
import qualified Data.Set as Set
import Lafont.Either (branchRight)
import Lafont.Format.GraphViz
  ( DotFile
  , NodeID
  , graphToDotFile
  , setNodeColour
  , unsafeToNodeID
  , unwrapNodeID
  )
import Lafont.Graph
  ( Digraph
  , findCone
  , induceSubgraph
  )
import Lafont.Rewrite.Abstraction
  ( AbsDerivation (..)
  , UnmetDep
  , derivationToGraph
  , unwrapDepGraph
  )
import Lafont.Rewrite.Summary
  ( meta
  , propName
  , propType
  )
import TietzeExe.IO.Configs (ColorMap)

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

-- | Consumes a set of derivation types and a list of node summaries. Returns a list of
-- nodes corresponding to derivations of the given types (as indicated by the node
-- summaries).
populateCone :: Set.Set String -> [NodeSummary] -> Set.Set String
populateCone _    []                          = Set.empty
populateCone tset (NodeSummary nid ty : list)
    | Set.member ty tset = Set.insert (unwrapNodeID nid) srcs
    | otherwise          = srcs
    where srcs = populateCone tset list

-- | Consumes a lists of derivation types (types), a list of node summaries (sums), and a
-- digraph (g) annotated by the node summaries. If the cone (c) induced in g from the set
-- of nodes selects by (populateCone types sum) is non-empty, then the induced subgrapth
-- is returned. Otherwise, the cone is ignored and g is returned.
applyCone :: [String] -> [NodeSummary] -> Digraph String -> Digraph String
applyCone types sum g
    | Set.null cone = g
    | otherwise     = induceSubgraph g cone
    where tset = Set.fromList types
          cone = findCone g $ populateCone tset sum

-- | Takes as input a list of derivations (derivations). If derivations contains an
-- umet dependency, then an UnmetDep error is returned. Otherwise, the derivation graph
-- associated with derivations is generated. This graph is then converted into a DotFile
-- with default styling. Finally, the DotFile is annotated with the NodeSummaries for all
-- derivations in derivations (see derivationsToDotSummary).
derivationsToDotFile :: [String] -> [AbsDerivation] -> Either UnmetDep AnnotatedDotFile
derivationsToDotFile types derivations =
    branchRight (derivationToGraph derivations) $ \deps ->
        let sum = derivationsToDotSummary derivations
            g   = unwrapDepGraph deps
            g'  = applyCone types sum g
            dot = graphToDotFile g' unsafeToNodeID
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
