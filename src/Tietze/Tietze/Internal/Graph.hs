-- | Internals for Graph. Enables unit testing.

module Tietze.Internal.Graph
  ( Digraph (..)
  , EdgeSet
  , GraphWalk
  , empty
  , fromList
  , listToWalk
  , edgeSet
  , adjacencyList
  , extendToCycle
  , findCycleFromVertex
  , findCycleFromVertices
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|), (|>))
import Tietze.Maybe
  ( branchNothing
  , maybeApply
  )

-----------------------------------------------------------------------------------------
-- * Graph Types

-- | Represents the set of edges originating a vertex v. Each element u represents a
-- directed edge (u, v).
type EdgeSet a = Set.Set a

-- | Represents a graph as a mapping from vertex names to edge sets.
newtype Digraph a = Digraph (Map.Map a (EdgeSet a)) deriving (Eq,Show)

-- | Represents a walk as a bidirectional sequence of vertices.
type GraphWalk a = Seq.Seq a

-----------------------------------------------------------------------------------------
-- * Graph Construction.

-- | Creates an empty edge set.
empty :: EdgeSet a
empty = Set.empty

-- | Creates an edge set from a list.
fromList :: (Ord a) => [a] -> EdgeSet a
fromList = Set.fromList

-- | Converts a list to a path.
listToWalk :: [a] -> GraphWalk a
listToWalk = Seq.fromList

-----------------------------------------------------------------------------------------
-- * Graph Inspection.

-- | Consumes a graph and a vertex. Returns the set of edges originating from the vertex.
edgeSet :: (Ord a) => Digraph a -> a -> EdgeSet a
edgeSet (Digraph g) v = fromMaybe empty (Map.lookup v g)

-- | Consumes a graph and a vertex. Returns the list of all adjacent vertices.
adjacencyList :: (Ord a) => Digraph a -> a -> [a]
adjacencyList g v = Set.toList (edgeSet g v)

-----------------------------------------------------------------------------------------
-- * Cycle Detection (Depth-First Search).

-- | Consumes a walk (w) and a vertex (v). If w starts and ends at the same vertex, then
-- w is returned. Otherwise, v is returned.
extendToCycle :: (Eq a) => GraphWalk a -> a -> GraphWalk a
extendToCycle Empty                v = Seq.singleton v
extendToCycle (a :<| Empty)        v = v <| (a <| Empty)
extendToCycle ((l :<| rest) :|> r) v
    | l == r    = seq
    | otherwise = v <| seq
    where seq = (l :<| rest) :|> r

-- | Consumes a graph (g), a vertex in the graph (v), and a set of previously visited
-- vertices (seen). That is, there exists a walk w to v that visits all vertices in seen.
-- If there exists a walk w' from v such that w' appended to w is a lasso (path++cycle),
-- the cycle is returned as a sequence of vertices. Otherwise, nothing is returned.
--
-- Note: If c is not unique, then this function returns the first such c discovered by a
-- depth-first search.
--
-- Note: Mutually depends on findCycleFromVertices
findCycleFromVertex :: (Ord a) => Digraph a -> a -> EdgeSet a -> Maybe (GraphWalk a)
findCycleFromVertex g v seen
    | v `Set.member` seen = Just (extendToCycle Empty v)
    | otherwise           = let frontier = adjacencyList g v
                                seenHere = Set.insert v seen
                                res = findCycleFromVertices g frontier seenHere
                            in maybeApply res (`extendToCycle` v)

-- | Consumes a graph (g), a frontier of vertices in the graph (list), and a set of
-- previously visited vertices (seen). That is, there exists a walk w to each v in list
-- that visits all vertices in seen. If for some v in list, there exists a walk w' from v
-- such that w' appended to w is a lasso (path++cycle), then cycle is returned as a
-- sequence of vertices. Otherwise, nothing is returned.
--
-- Note: If c is not unique, then this function returns the first such c discovered by a
-- depth-first search.
--
-- Note: Mutually depends on findCycleFromVertex.
findCycleFromVertices :: (Ord a) => Digraph a -> [a] -> EdgeSet a -> Maybe (GraphWalk a)
findCycleFromVertices _ []       _    = Nothing
findCycleFromVertices g (v:list) seen = branchNothing (findCycleFromVertex g v seen)
                                                      (findCycleFromVertices g list seen)
