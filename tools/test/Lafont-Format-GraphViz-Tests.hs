module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Lafont.Format.GraphViz
import Lafont.Format.Internal.GraphViz
import Lafont.Graph

-----------------------------------------------------------------------------------------
-- toNodeID

test1 = TestCase (assertEqual "toNodeID rejects empty strings."
                              (Left EmptyToken)
                              (toNodeID ""))

test2 = TestCase (assertEqual "toNodeID rejects strings with spaces."
                              (Left $ UnexpectedChar ' ')
                              (toNodeID "my vertex"))

test3 = TestCase (assertEqual "toNodeID rejects strings with dashes."
                              (Left $ UnexpectedChar '-')
                              (toNodeID "my-vertex"))

test4 = TestCase (assertEqual "toNodeID valid case (1/2)."
                              (Right $ NodeID "my_vertex")
                              (toNodeID "my_vertex"))

test5 = TestCase (assertEqual "toNodeID valid case (2/2)."
                              (Right $ NodeID "Another_Vertex_123")
                              (toNodeID "Another_Vertex_123"))

-----------------------------------------------------------------------------------------
-- toColour

test6 = TestCase (assertEqual "toColour rejects empty strings."
                              (Left EmptyToken)
                              (toColour ""))

test7 = TestCase (assertEqual "toColour rejects uppercase characters."
                              (Left $ UpperCase 'A')
                              (toColour "Aliceblue"))

test8 = TestCase (assertEqual "toColour rejects strings with spaces."
                              (Left $ UnexpectedChar ' ')
                              (toColour "alice blue"))

test9 = TestCase (assertEqual "toColour rejects strings with dashes."
                              (Left $ UnexpectedChar '-')
                              (toColour "alice-blue"))

test10 = TestCase (assertEqual "toColour rejects strings with underscores."
                               (Left $ UnexpectedChar '_')
                               (toColour "alice_blue"))

test11 = TestCase (assertEqual "toColour with valid colour (1/2)."
                               (Right $ X11Color "aliceblue")
                               (toColour "aliceblue"))

test12 = TestCase (assertEqual "toColour with valid colour (2/2)."
                               (Right $ X11Color "azure4")
                               (toColour "azure4"))

-----------------------------------------------------------------------------------------
-- DotFile without Attributes

g0   = nullgraph
g1   = addVertex g0 "v1"
g2   = addVertex g1 "v2"
g3   = addVertex g2 "node1"
g4   = addVertex g3 "node2"
g5   = fromJust $ addEdge g4 "v1" "v2"
g6   = fromJust $ addEdge g5 "v1" "node1"
g7   = fromJust $ addEdge g6 "node1" "node1"
g8   = fromJust $ addEdge g7 "node2" "v2"
g9   = fromJust $ addEdge g8 "node2" "node1"
dot1 = graphToDotFile g9 unsafeToNodeID

test13 = TestCase (assertEqual "Can print a Dot file without attributes."
                               body
                               (printDotFile dot1))
    where body = "strict digraph {" ++
                 "node1;" ++
                 "node2;" ++
                 "v1;" ++
                 "v2;" ++
                 "node1 -> node1;" ++
                 "node2 -> node1;" ++
                 "node2 -> v2;" ++
                 "v1 -> node1;" ++
                 "v1 -> v2;" ++
                 "}"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "toTokenID_EmptyErr" test1,
                                     TestLabel "toTokenID_SpaceErr" test2,
                                     TestLabel "toTokenID_DashErr" test3,
                                     TestLabel "toTokenID_Valid_1" test4,
                                     TestLabel "toTokenID_Valid_2" test5,
                                     TestLabel "toColour_EmptyErr" test6,
                                     TestLabel "toColour_UpperErr" test7,
                                     TestLabel "toColour_SpaceErr" test8,
                                     TestLabel "toColour_DashErr" test9,
                                     TestLabel "toColour_UnderscoreErr" test10,
                                     TestLabel "toColour_Valid_1" test11,
                                     TestLabel "toColour_Valid_2" test12,
                                     TestLabel "printDotFile_NoAttrs" test13]

main = defaultMain tests
