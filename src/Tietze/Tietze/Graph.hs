-- | This module provides a type to stored (finite) directed graphs with named vertices.
-- Functions, such as cycle detection, are provided to carry out computations on graphs.

module Tietze.Graph
  ( Digraph
  , EdgeSet
  , GraphWalk
  , empty
  , edgeSet
  , adjacencyList
  , applyToGraph
  , nullgraph
  , addVertex
  , addEdge
  , vertexList
  , foldPath
  , findCone
  , findCycle
  , induceSubgraph
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Tietze.Internal.Graph
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
  )

-----------------------------------------------------------------------------------------
-- * Graph Construction.

-- | Create an empty (null) graph.
nullgraph :: (Ord a) => Digraph a
nullgraph = Digraph Map.empty

-- | Consumes a graph (g) and a vertex (v). Returns a new graph obtained by adding v to
-- g. If v is already in g, then g is returned.
addVertex :: (Ord a) => Digraph a -> a -> Digraph a
addVertex (Digraph g) v
    | v `Map.member` g = Digraph g
    | otherwise        = Digraph (Map.insert v empty g)

-- | Consumes a graph (g) and two vertices (u, v). If u and v are vertices in g, then new
-- graph obtained by adding edge (u, v) to g is returned. Otherwise, nothing is returned.
addEdge :: (Ord a) => Digraph a -> a -> a -> Maybe (Digraph a)
addEdge (Digraph g) u v
    | edgesExist = Just (Digraph (Map.adjust (Set.insert v) u g))
    | otherwise  = Nothing
    where edgesExist = u `Map.member` g && v `Map.member` g

-- | Consumes a graph (g) with vertices of type A, and a map f from type A to type B.
-- Returns the graph obtained by applying f to each vertex in g. Note that if f is
-- bijective when restricted to the vertex set of g, then the graphs will be isomorphic.
applyToGraph :: (Ord a, Ord b) => Digraph a -> (a -> b) -> Digraph b
applyToGraph (Digraph g0) f = Digraph g2
    where g1 = Map.mapKeysWith Set.union f g0
          g2 = Map.map (Set.map f) g1

-----------------------------------------------------------------------------------------
-- * Graph Inspection.

-- | Consumes a graph. Returns the list of vertices in the graph.
vertexList :: Digraph a -> [a]
vertexList (Digraph g) = Map.keys g

-- | Consumes a function of type a -> b -> b (f), a walk of type a (w), and a value of
-- type b. If w is the empty path, then b is returned. Otherwise, if w = vw' where v is
-- the first vertex in w, then (f v (foldpath f w' b)) is returned.
foldPath :: (Int -> a -> b -> b) -> b -> GraphWalk a -> b
foldPath = Seq.foldrWithIndex

-----------------------------------------------------------------------------------------
-- * Cycle Detection (Depth-First Search).

-- | Consumes a digraph. If the digraph has at least one cycle, then that cycle is
-- returned as a sequence of vertices. Otherwise, nothing is returned.
--
-- Note: If the cycle is not unique, then this function returns the first cycle
-- discovered by a depth-first search.
findCycle :: (Ord a) => Digraph a -> Maybe (GraphWalk a)
findCycle g = findCycleFromVertices g (vertexList g) empty

-----------------------------------------------------------------------------------------
-- * Dependency Analysis

-- | Consumes a graph and a set of vertices. Returns the subgraph induced by
-- the vertex set.
induceSubgraph :: (Ord a) => Digraph a -> Set.Set a -> Digraph a
induceSubgraph (Digraph adj) nodes = Digraph adj''
    where adj'  = Map.filterWithKey (\k _ -> Set.member k nodes) adj
          adj'' = Map.map (Set.intersection nodes) adj'

-- | Consumes a vertex (node), the set of vertices reachable from node (children), and
-- the set of elements in the cone (cone). If node is in the cone, then the children are
-- added to the cone and the new cone is returned. Otherwise, the cone is returned
-- unmodified.
foldChildren :: (Ord a) => a -> Set.Set a -> Set.Set a -> Set.Set a
foldChildren node children cone
    | Set.member node cone = Set.union cone children
    | otherwise            = cone

-- | Implementation details for findCone. The underlying adjacency map is used,
findConeImpl :: (Ord a) => Map.Map a (EdgeSet a) -> Set.Set a -> Set.Set a
findConeImpl adj cone
    | cone == cone' = cone'
    | otherwise     = findConeImpl adj cone'
    where cone' = Map.foldrWithKey foldChildren cone adj

-- | Consumes a graph g and a set of source vertices (which may or may not appear in g).
-- Returns the set of all vertices in g reachable from the set of source vertices.
findCone :: (Ord a) => Digraph a -> Set.Set a -> Set.Set a
findCone (Digraph adj) srcs = findConeImpl adj srcs'
    where srcs' = Set.intersection srcs $ Set.fromList $ Map.keys adj
