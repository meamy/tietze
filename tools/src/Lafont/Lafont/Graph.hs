-- | This module provides a type to stored (finite) directed graphs with named vertices.
-- Functions, such as cycle detection, are provided to carry out computations on graphs.

module Lafont.Graph where

import qualified Data.Set
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
