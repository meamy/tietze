-- | This module provides a type to stored (finite) directed graphs with named vertices.
-- Functions, such as cycle detection, are provided to carry out computations on graphs.

module Lafont.Graph where

import qualified Data.Set
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence
import qualified Data.Map
import Data.Maybe
import Lafont.Maybe

-----------------------------------------------------------------------------------------
-- * Graph Types

-- | Represents the set of edges originating a vertex v. Each element u represents a
-- directed edge (u, v).
type EdgeSet a = Data.Set.Set a

-- | Represents a graph as a mapping from vertex names to edge sets.
type Digraph a = Data.Map.Map a (EdgeSet a)

-- | Represents a walk as a bidirectional sequence of vertices.
type GraphWalk a = Data.Sequence.Seq a

-----------------------------------------------------------------------------------------
-- * Graph Construction.

-- | Creates an empty edge set.
empty :: EdgeSet a
empty = Data.Set.empty

-- | Creates an edge set from a list.
fromList :: (Ord a) => [a] -> EdgeSet a
fromList list = Data.Set.fromList list

-- | Create an empty (null) graph.
nullgraph :: (Ord a) => Digraph a
nullgraph = Data.Map.empty

-- | Consumes a graph (g) and a vertex (v). Returns a new graph obtained by adding v to
-- g. If v is already in g, then g is returned.
addVertex :: (Ord a) => Digraph a -> a -> Digraph a
addVertex g v = if (Data.Map.member v g)
                then g
                else Data.Map.insert v empty g

-- | Consumes a graph (g) and two vertices (u, v). If u and v are vertices in g, then new
-- graph obtained by adding edge (u, v) to g is returned. Otherwise, nothing is returned.
addEdge :: (Ord a) => Digraph a -> a -> a -> Maybe (Digraph a)
addEdge g u v = if edgesExist
                then Just (Data.Map.adjust (\edges -> Data.Set.insert v edges) u g)
                else Nothing
    where edgesExist = (Data.Map.member u g) && (Data.Map.member v g)

-- | Converts a list to a path.
listToWalk :: [a] -> GraphWalk a
listToWalk list = Data.Sequence.fromList list

-----------------------------------------------------------------------------------------
-- * Graph Inspection.

-- | Consumes a graph. Returns the list of vertices in the graph.
vertexList :: Digraph a -> [a]
vertexList g = Data.Map.keys g

-- | Consumes a graph and a vertex. Returns the set of edges originating from the vertex.
edgeSet :: (Ord a) => Digraph a -> a -> EdgeSet a
edgeSet g v = fromMaybe empty (Data.Map.lookup v g)

-- | Consumes a graph and a vertex. Returns the list of all adjacent vertices.
adjacencyList :: (Ord a) => Digraph a -> a -> [a]
adjacencyList g v = Data.Set.toList (edgeSet g v)

-----------------------------------------------------------------------------------------
-- * Cycle Detection (Depth-First Search).

-- | Consumes a walk (w) and a vertex (v). If w starts and ends at the same vertex, then
-- w is returned. Otherwise, v is returned.
extendToCycle :: (Eq a) => GraphWalk a -> a -> GraphWalk a
extendToCycle Empty                v = Data.Sequence.singleton v
extendToCycle (a :<| Empty)        v = v <| (a <| Empty)
extendToCycle ((l :<| rest) :|> r) v = let seq = ((l :<| rest) :|> r)
                                       in if (l == r) then seq else (v <| seq)

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
findCycleFromVertex g v seen = if (v `Data.Set.member` seen)
                               then Just (extendToCycle Empty v)
                               else let frontier = adjacencyList g v
                                        seenHere = Data.Set.insert v seen
                                        res = findCycleFromVertices g frontier seenHere
                                    in maybeApply (\path -> extendToCycle path v) res

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
findCycleFromVertices g (v:list) seen =
    case (findCycleFromVertex g v seen) of
        Nothing    -> findCycleFromVertices g list seen
        Just cycle -> Just cycle

-- | Consumes a digraph. If the digraph has at least one cycle, then that cycle is
-- returned as a sequence of vertices. Otherwise, nothing is returned.
--
-- Note: If the cycle is not unique, then this function returns the first cycle
-- discovered by a depth-first search.
findCycle :: (Ord a) => Digraph a -> Maybe (GraphWalk a)
findCycle g = findCycleFromVertices g (vertexList g) empty
