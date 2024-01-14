-- | Implementation details for GraphViz.

module Lafont.Format.Internal.GraphViz (
    NodeID ( .. ),
    X11Color ( .. )
) where

-----------------------------------------------------------------------------------------
-- * Valid Vertex Name.

-- | A wrapper type for strings which also form valid NodeID's. This type permits a
-- restricted set of the NodeID types supported by the Dot file format. For example, the
-- HTML string format is not supported. In particular, NodeID strings may consist of only
-- alphanumeric symbols and underscores.
newtype NodeID = NodeID String deriving (Eq,Show,Ord)

-----------------------------------------------------------------------------------------
-- * Valid Colour Name.

-- | A wrapper type for the X11Color strings supported by the Dot file format.
newtype X11Color = X11Color String deriving (Eq,Show)
