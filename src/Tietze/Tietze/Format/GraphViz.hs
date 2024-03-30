-- | Provides an interface to build and print GraphViz files in the DOT format.

module Tietze.Format.GraphViz
  ( Display (..)
  , DotFile
  , DotParseError
  , NodeID
  , X11Color
  , graphToDotFile
  , printDotFile
  , setNodeColour
  , toColour
  , toNodeID
  , unsafeToNodeID
  , unwrapNodeID
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map

import Data.Char
  ( isAlphaNum
  , isUpper
  )
import Tietze.Common (Display (..))
import Tietze.Format.Internal.GraphViz
  ( DotParseError (..)
  , NodeID (..)
  , X11Color (..)
  )
import Tietze.Graph
  ( Digraph
  , adjacencyList
  , applyToGraph
  , vertexList
  )

-----------------------------------------------------------------------------------------
-- * Conversion Errors.

instance Display DotParseError where
    display (UpperCase c)      = "Unexpected uppercase character " ++ [c] ++ "."
    display (UnexpectedChar c) = "Unxpected character " ++ [c] ++ "."
    display EmptyToken         = "Expected non-empty token."

-----------------------------------------------------------------------------------------
-- * Valid Vertex Name.

-- | Consumes a string str. If str consists of valid NodeID symbols, then nothing is
-- returned. Otherwise, the first invalid character is produced.
checkNodeID :: String -> Maybe DotParseError
checkNodeID []        = Nothing
checkNodeID ('_':str) = checkNodeID str
checkNodeID (c:str)
    | isAlphaNum c = checkNodeID str
    | otherwise    = Just $ UnexpectedChar c

-- | Consumes a string str. If str consists of valid NodeID symbols, then str is promoted
-- to a NodeID. Otherwise, the first invalid character is returned.
toNodeID :: String -> Either DotParseError NodeID
toNodeID ""  = Left EmptyToken
toNodeID str =
    case checkNodeID str of
        Just err -> Left err
        Nothing  -> Right $ NodeID str

-- | Implementation of toNodeID which fails on error.
unsafeToNodeID :: String -> NodeID
unsafeToNodeID str =
    case toNodeID str of
        Left err -> error $ "Failure to lift string to NodeID: " ++ display err
        Right id -> id

-- | Returns the underlying string of a NodeID.
unwrapNodeID :: NodeID -> String
unwrapNodeID (NodeID str) = str

-----------------------------------------------------------------------------------------
-- * Valid Colour Name.

-- | The Dot file format lists valid X11 colours, but does not say what will happen if
-- the colour is unsupported. However, all colours are lowercase, and consist of only
-- alphanumeric symbols. This function consumes a string, and ensures that the string
-- adheres to these two rules. If the fails to adhere to these rules, then the
-- corresponding X11Error is returned. Otherwise, nothing is returned.
checkColour :: String -> Maybe DotParseError
checkColour []      = Nothing
checkColour (c:str)
    | isUpper c    = Just $ UpperCase c
    | isAlphaNum c = checkColour str
    | otherwise    = Just $ UnexpectedChar c

-- | Consumes a string str. If str could be a valid X11 colour within the Dot file
-- format, then then str is promoted to an X11Color. Otherwise, an X11Error is returned,
-- describing the reason for which str failed to promote to an X11Color.
toColour :: String -> Either DotParseError X11Color
toColour ""  = Left EmptyToken
toColour str =
    case checkColour str of
        Just err -> Left err
        Nothing  -> Right $ X11Color str

-----------------------------------------------------------------------------------------
-- * Vertex Formatting Utilities.

-- | Mapping from Dot file vertices to their attributes.
type NodeAttrMap = Map.Map NodeID X11Color

-- | Implements printNodes for the attributes of a specific node.
printAttrs :: NodeAttrMap -> NodeID -> String
printAttrs nattrs u =
    case Map.lookup u nattrs of
        Nothing            -> ""
        Just (X11Color c)  -> " [style=filled,fillcolor=" ++ c ++ "]"

-- | Implements printNodes for a specific vertex.
printNode :: NodeAttrMap -> NodeID -> String
printNode nattrs u@(NodeID name) = name ++ attrStr ++ ";"
    where attrStr = printAttrs nattrs u

-- | Implements printNodes for a list of vertices.
printNodesImpl :: NodeAttrMap -> [NodeID] -> String
printNodesImpl _      []           = ""
printNodesImpl nattrs (node:nodes) = nodeStr ++ printNodesImpl nattrs nodes
    where nodeStr = printNode nattrs node

-- | Consumes an attribute map for NodeID's (nattrs), together with a digraph of
-- NodeID's. Returns a string consisting of the substrings "u [color={c}];" for each
-- vertex in the graph. If c is not associated with any attributes in nattrs, then the
-- substring "u;" is generated for u instead.
printNodes :: NodeAttrMap -> Digraph NodeID -> String
printNodes nattrs g = printNodesImpl nattrs $ vertexList g

-----------------------------------------------------------------------------------------
-- * Edge Formatting Utilities.

-- | Implements printEdge for a specific source node and a specific destination node.
printEdge :: NodeID -> NodeID -> String
printEdge (NodeID u) (NodeID v) = u ++ " -> " ++ v ++ ";"

-- | Implements printEdge for a specific source node and a list of destinations.
printAdjEdges :: NodeID -> [NodeID] -> String
printAdjEdges _   []         = ""
printAdjEdges src (dst:dsts) = edgeStr ++ printAdjEdges src dsts
    where edgeStr = printEdge src dst

-- | Implements printEdge for a list of source nodes.
printEdgesFor :: Digraph NodeID -> [NodeID] -> String
printEdgesFor _ []           = ""
printEdgesFor g (node:nodes) = adjStr ++ printEdgesFor g nodes
    where adjStr = printAdjEdges node $ adjacencyList g node

-- | Takes as input a digraph of NodeID's. Returns a string consisting of the substrings
-- "u -> v;" for each edge (u, v) in the graph.
printEdges :: Digraph NodeID -> String
printEdges g = printEdgesFor g $ vertexList g

-----------------------------------------------------------------------------------------
-- * DotFile Format.

-- | Restricted description of a Dot file. In particular, this type supports strict
-- digraphs in which a restricted set of attribtes are allowed for the nodes and edges. 
data DotFile = DotFile (Digraph NodeID) NodeAttrMap

-- | Takes as input a digraph with vertices of type a (g), together with a function from
-- vertices of type a to NodeID's (f). Returns a DotFile for the graph obtained by
-- replacing each vertex v in g with f(v). The DotFile begin without any attributes.
graphToDotFile :: (Ord a) => Digraph a -> (a -> NodeID) -> DotFile
graphToDotFile g toNode = DotFile g' Map.empty
    where g' = applyToGraph g toNode

-- | Takes as input a dot file (d), the name of a node in the graph (v), and an X11
-- colour (c). Returns a new dot file obtained by setting to fill colour for v to c in d.
setNodeColour :: DotFile -> NodeID -> X11Color -> DotFile
setNodeColour (DotFile g nattrs) v c = DotFile g nattrs'
    where nattrs' = Map.insert v c nattrs

-- | Converts a DotFile to the text of the corresponding DotFile.
printDotFile :: DotFile -> String
printDotFile (DotFile g nattrs) = "strict digraph {" ++ nstr ++ estr ++ "}"
    where nstr = printNodes nattrs g
          estr = printEdges g
