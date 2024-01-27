module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.List
import Data.Maybe
import Data.Set as Set
import Lafont.Graph
import Lafont.Internal.Graph

-----------------------------------------------------------------------------------------
-- Tests the ability to build and inspect a graph.

g0 :: Digraph Int
g0  = nullgraph
g1  = addVertex g0 1
g2  = addVertex g1 5
g3  = fromJust $ addEdge g2 1 5
g4  = addVertex g3 12
g5  = fromJust $ addEdge g4 1 12
g6  = fromJust $ addEdge g5 12 12
g7  = addVertex g6 7
g8  = fromJust $ addEdge g7 7 5
g9  = fromJust $ addEdge g8 7 12
g10 = addVertex g9 15
g11 = fromJust $ addEdge g10 5 15
g12 = addVertex g11 (-2)
g13 = fromJust $ addEdge g12 15 (-2)
g14 = fromJust $ addEdge g13 (-2) 7
g15 = fromJust $ addEdge g14 (-2) 12

makeVertexTest :: Digraph Int -> [Int] -> String -> Test.HUnit.Test
makeVertexTest g exp name = TestCase (assertEqual msg (sort exp) (sort (vertexList g)))
    where msg = "The vertices in " ++ name ++ " are correct."

test1 = makeVertexTest g0 [] "g0"
test2 = makeVertexTest g1 [1] "g1"
test3 = makeVertexTest g2 [1, 5] "g2"
test4 = makeVertexTest g3 [1, 5] "g3"
test5 = makeVertexTest g4 [1, 5, 12] "g4"
test6 = makeVertexTest g5 [1, 5, 12] "g5"
test7 = makeVertexTest g6 [1, 5, 12] "g6"
test8 = makeVertexTest g7 [1, 5, 12, 7] "g7"
test9 = makeVertexTest g8 [1, 5, 12, 7] "g8"
test10 = makeVertexTest g9 [1, 5, 12, 7] "g9"
test11 = makeVertexTest g10 [1, 5, 12, 7, 15] "g10"
test12 = makeVertexTest g11 [1, 5, 12, 7, 15] "g11"
test13 = makeVertexTest g12 [1, 5, 12, 7, 15, (-2)] "g12"
test14 = makeVertexTest g13 [1, 5, 12, 7, 15, (-2)] "g13"
test15 = makeVertexTest g14 [1, 5, 12, 7, 15, (-2)] "g14"
test16 = makeVertexTest g15 [1, 5, 12, 7, 15, (-2)] "g15"

makeEdgeTest :: Digraph Int -> Int -> [Int] -> String -> Test.HUnit.Test
makeEdgeTest g v edges name = TestCase (assertEqual msg exp act)
    where msg = "The edges in " ++ name ++ " are correct from " ++ (show v) ++ "."
          exp = Lafont.Internal.Graph.fromList edges
          act = edgeSet g v

test17 = makeEdgeTest g0 1 [] "g0"
test18 = makeEdgeTest g1 1 [] "g1"
test19 = makeEdgeTest g2 1 [] "g2"
test20 = makeEdgeTest g2 5 [] "g2"
test21 = makeEdgeTest g3 1 [5] "g3"
test22 = makeEdgeTest g3 5 [] "g3"
test23 = makeEdgeTest g4 1 [5] "g4"
test24 = makeEdgeTest g4 5 [] "g4"
test25 = makeEdgeTest g4 12 [] "g4"
test26 = makeEdgeTest g5 1 [5, 12] "g5"
test27 = makeEdgeTest g5 5 [] "g5"
test28 = makeEdgeTest g5 12 [] "g5"
test29 = makeEdgeTest g6 1 [5, 12] "g6"
test30 = makeEdgeTest g6 5 [] "g6"
test31 = makeEdgeTest g6 12 [12] "g6"
test32 = makeEdgeTest g7 1 [5, 12] "g7"
test33 = makeEdgeTest g7 5 [] "g7"
test34 = makeEdgeTest g7 12 [12] "g7"
test35 = makeEdgeTest g7 7 [] "g7"
test36 = makeEdgeTest g8 1 [5, 12] "g8"
test37 = makeEdgeTest g8 5 [] "g8"
test38 = makeEdgeTest g8 12 [12] "g8"
test39 = makeEdgeTest g8 7 [5] "g8"
test40 = makeEdgeTest g9 1 [5, 12] "g9"
test41 = makeEdgeTest g9 5 [] "g9"
test42 = makeEdgeTest g9 12 [12] "g9"
test43 = makeEdgeTest g9 7 [5, 12] "g9"
test44 = makeEdgeTest g10 1 [5, 12] "g10"
test45 = makeEdgeTest g10 5 [] "g10"
test46 = makeEdgeTest g10 12 [12] "g10"
test47 = makeEdgeTest g10 7 [5, 12] "g10"
test48 = makeEdgeTest g10 15 [] "g10"
test49 = makeEdgeTest g11 1 [5, 12] "g11"
test50 = makeEdgeTest g11 5 [15] "g11"
test51 = makeEdgeTest g11 12 [12] "g11"
test52 = makeEdgeTest g11 7 [5, 12] "g11"
test53 = makeEdgeTest g11 15 [] "g11"
test54 = makeEdgeTest g12 1 [5, 12] "g12"
test55 = makeEdgeTest g12 5 [15] "g12"
test56 = makeEdgeTest g12 12 [12] "g12"
test57 = makeEdgeTest g12 7 [5, 12] "g12"
test58 = makeEdgeTest g12 15 [] "g12"
test59 = makeEdgeTest g12 (-2) [] "g12"
test60 = makeEdgeTest g13 1 [5, 12] "g13"
test61 = makeEdgeTest g13 5 [15] "g13"
test62 = makeEdgeTest g13 12 [12] "g13"
test63 = makeEdgeTest g13 7 [5, 12] "g13"
test64 = makeEdgeTest g13 15 [(-2)] "g13"
test65 = makeEdgeTest g13 (-2) [] "g13"
test66 = makeEdgeTest g14 1 [5, 12] "g14"
test67 = makeEdgeTest g14 5 [15] "g14"
test68 = makeEdgeTest g14 12 [12] "g14"
test69 = makeEdgeTest g14 7 [5, 12] "g14"
test70 = makeEdgeTest g14 15 [(-2)] "g14"
test71 = makeEdgeTest g14 (-2) [7] "g14"
test72 = makeEdgeTest g15 1 [5, 12] "g15"
test73 = makeEdgeTest g15 5 [15] "g15"
test74 = makeEdgeTest g15 12 [12] "g15"
test75 = makeEdgeTest g15 7 [5, 12] "g15"
test76 = makeEdgeTest g15 15 [(-2)] "g15"
test77 = makeEdgeTest g15 (-2) [7, 12] "g15"

-----------------------------------------------------------------------------------------
-- Tests idempotence of graph building.

test78 = TestCase (assertEqual "Adding vertices to a graph is an idempotent operation."
                               g15
                               (addVertex g15 15))

test79 = TestCase (assertEqual "Adding edges to a graph is an idempotent operation."
                               (Just g15 :: Maybe (Digraph Int))
                               (addEdge g15 (-2) 12))

-----------------------------------------------------------------------------------------
-- Tests that edges must be between existing vertices.

test80 = TestCase (assertEqual "Adding edges to a graph is an idempotent operation."
                               (Nothing :: Maybe (Digraph Int))
                               (addEdge g15 90 1))

test81 = TestCase (assertEqual "Adding edges to a graph is an idempotent operation."
                               (Nothing :: Maybe (Digraph Int))
                               (addEdge g15 1 90))

-----------------------------------------------------------------------------------------
-- Tests extension to cycle.

walk0 = listToWalk []
walk1 = listToWalk [1]
walk2 = listToWalk [2, 1]
walk3 = listToWalk [3, 2, 1]
walk4 = listToWalk [4, 3, 2, 1]
walk5 = listToWalk [5, 4, 3, 2, 1]
walk6 = listToWalk [1, 5, 4, 3, 2, 1]

test82 = TestCase (assertEqual "Tests extending an empty walk to a cycle."
                               walk1
                               (extendToCycle walk0 1))

test83 = TestCase (assertEqual "Tests extending one element walk to a cycle."
                               walk2
                               (extendToCycle walk1 2))

test84 = TestCase (assertEqual "Tests extending 2+ element walk to a cycle (1/4)."
                               walk3
                               (extendToCycle walk2 3))

test85 = TestCase (assertEqual "Tests extending 2+ element walk to a cycle (2/4)."
                               walk4
                               (extendToCycle walk3 4))

test86 = TestCase (assertEqual "Tests extending 2+ element walk to a cycle (3/4)."
                               walk5
                               (extendToCycle walk4 5))

test87 = TestCase (assertEqual "Tests extending 2+ element walk to a cycle (4/4)."
                               walk6
                               (extendToCycle walk5 1))

test88 = TestCase (assertEqual "Tests extending a cycle to a cycle."
                               walk6
                               (extendToCycle walk6 5))

-----------------------------------------------------------------------------------------
-- Tests cycle detection.

g16 = addVertex g15 100
g17 = addVertex g16 200
g18 = fromJust $ addEdge g17 1 100
g19 = fromJust $ addEdge g18 100 200

cycleFromV1 = listToWalk [5, 15, -2, 7, 5]

makeVertexCycleTest :: Int -> GraphWalk Int -> Test.HUnit.Test
makeVertexCycleTest v path = TestCase (assertEqual msg exp act)
    where msg = "Can detect path from vertex " ++ (show v ) ++ "."
          exp = Just path :: Maybe (GraphWalk Int)
          act = findCycleFromVertex g19 v Lafont.Internal.Graph.empty

test89 = makeVertexCycleTest 12 (listToWalk [12, 12])
test90 = makeVertexCycleTest (-2) (listToWalk [-2, 7, 5, 15, -2])
test91 = makeVertexCycleTest 7 (listToWalk [7, 5, 15, -2, 7])
test92 = makeVertexCycleTest 5 (listToWalk [5, 15, -2, 7, 5])
test93 = makeVertexCycleTest 15 (listToWalk [15, -2, 7, 5, 15])
test94 = makeVertexCycleTest 1 cycleFromV1

test95 = TestCase (assertEqual "findCycleFromVertex handles vertex without cycle (1/2)."
                               (Nothing :: Maybe (GraphWalk Int))
                               act)
    where act = findCycleFromVertex g19 100 Lafont.Internal.Graph.empty

test96 = TestCase (assertEqual "findCycleFromVertex handles vertex without cycle (2/2)."
                               (Nothing :: Maybe (GraphWalk Int))
                               act)
    where act = findCycleFromVertex g19 200 Lafont.Internal.Graph.empty

test97 = TestCase (assertEqual "findCycleFromVertex handles invalid vertex."
                               (Nothing :: Maybe (GraphWalk Int))
                               act)
    where act = findCycleFromVertex g19 300 Lafont.Internal.Graph.empty

test98 = TestCase (assertEqual "findCycleFromVertices returns first cycle."
                               (Just cycleFromV1 :: Maybe (GraphWalk Int))
                               act)
    where act = findCycleFromVertices g19 [100, 1, 12] Lafont.Internal.Graph.empty

test99 = TestCase (assertEqual "findCycleFromVertices handles vertices without cycles."
                               (Nothing :: Maybe (GraphWalk Int))
                               act)
    where act = findCycleFromVertices g19 [100, 200, 300] Lafont.Internal.Graph.empty

test100 = TestCase (assertBool "Can take a graph and return a cycle."
                               (isJust $ findCycle g19))

test101 = TestCase (assertBool "Can support graphs without cycles."
                               (isNothing $ findCycle g5))

-----------------------------------------------------------------------------------------
-- Ability to fold paths.

test102 = TestCase (assertEqual "Ensures that paths can be folded."
                    "2 5 8 11 14 17 |"
                    (foldPath (\n v str -> (show (n + v)) ++ " " ++ str) "|" seq))
    where seq = listToWalk [2, 4, 6, 8, 10, 12]

-----------------------------------------------------------------------------------------
-- Ability to lift set homs to graph homs.

h0 :: Digraph String
h0  = nullgraph
h1  = addVertex h0 "1"
h2  = addVertex h1 "5"
h3  = fromJust $ addEdge h2 "1" "5"
h4  = addVertex h3 "12"
h5  = fromJust $ addEdge h4 "1" "12"
h6  = fromJust $ addEdge h5 "12" "12"
h7  = addVertex h6 "7"
h8  = fromJust $ addEdge h7 "7" "5"
h9  = fromJust $ addEdge h8 "7" "12"
h10 = addVertex h9 "15"
h11 = fromJust $ addEdge h10 "5" "15"
h12 = addVertex h11 "-2"
h13 = fromJust $ addEdge h12 "15" "-2"
h14 = fromJust $ addEdge h13 "-2" "7"
h15 = fromJust $ addEdge h14 "-2" "12"

k0 :: Digraph String
k0 = nullgraph
k1 = addVertex k0 "1"
k2 = fromJust $ addEdge k1 "1" "1"
k3 = addVertex k2 "7"
k4 = fromJust $ addEdge k3 "1" "7"
k5 = fromJust $ addEdge k4 "7" "7"
k6 = fromJust $ addEdge k5 "7" "1"
k7 = addVertex k6 "15"
k8 = fromJust $ addEdge k7 "1" "15"
k9 = fromJust $ addEdge k8 "15" "7"

graphHom :: Int -> String
graphHom 5    = "1"
graphHom 12   = "7"
graphHom (-2) = "7"
graphHom n    = show n

test103 = TestCase (assertEqual "Ensures that set id maps lift to graph id maps (1/2)."
                                g7
                                (applyToGraph g7 id))

test104 = TestCase (assertEqual "Ensures that set id maps lift to graph id maps (2/2)."
                                g15
                                (applyToGraph g15 id))

test105 = TestCase (assertEqual "Ensures that set bijections lift to graph isos (1/2)."
                                h7
                                (applyToGraph g7 show))

test106 = TestCase (assertEqual "Ensures that set bijections lift to graph isos (2/2)."
                                h15
                                (applyToGraph g15 show))

test107 = TestCase (assertEqual "Ensures that non-injective set maps lift to graph homs."
                                k9
                                (applyToGraph g15 graphHom))

-----------------------------------------------------------------------------------------
-- Ability to compute cones.

simpleCone0 :: Digraph Int
simpleCone0 = nullgraph
simpleCone1 = addVertex simpleCone0 0
simpleCone2 = addVertex simpleCone1 1
simpleCone3 = addVertex simpleCone2 2
simpleCone4 = fromJust $ addEdge simpleCone3 0 1
simpleCone5 = fromJust $ addEdge simpleCone4 0 2

doubleCone0 = addVertex simpleCone5 10
doubleCone1 = addVertex doubleCone0 11
doubleCone2 = addVertex doubleCone1 12
doubleCone3 = fromJust $ addEdge doubleCone2 10 11
doubleCone4 = fromJust $ addEdge doubleCone3 10 12

cyclicCone0 = fromJust $ addEdge doubleCone4 1 2
cyclicCone1 = fromJust $ addEdge cyclicCone0 2 1

deepCone0 = addVertex cyclicCone1 3
deepCone1 = addVertex deepCone0 4
deepCone2 = addVertex deepCone1 5
deepCone3 = fromJust $ addEdge deepCone2 2 3
deepCone4 = fromJust $ addEdge deepCone3 3 4
deepCone5 = fromJust $ addEdge deepCone4 4 5

test108 = TestCase (assertEqual "Empty cones are handled properly."
                                Set.empty
                                (findCone simpleCone5 sources))
    where sources = Set.fromList [5, 6, 7]

test109 = TestCase (assertEqual "Singleton cones are handled properly."
                                sources
                                (findCone simpleCone5 sources))
    where sources = Set.fromList [1]

test110 = TestCase (assertEqual "A simple connected cone is handled properly."
                                cone
                                (findCone simpleCone5 sources))
    where sources = Set.fromList [0]
          cone    = Set.fromList [0, 1, 2]

test111 = TestCase (assertEqual "A disconnected cone in a disconnected graph."
                                cone
                                (findCone doubleCone4 sources))
    where sources = Set.fromList [0, 10]
          cone    = Set.fromList [0, 1, 2, 10, 11, 12]

test112 = TestCase (assertEqual "Can compute cones along paths of length greater than 1."
                                cone
                                (findCone deepCone5 sources))
    where sources = Set.fromList [0]
          cone    = Set.fromList [0, 1, 2, 3, 4, 5]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Construction_Vertices_G0" test1,
                                     TestLabel "Construction_Vertices_G1" test2,
                                     TestLabel "Construction_Vertices_G2" test3,
                                     TestLabel "Construction_Vertices_G3" test4,
                                     TestLabel "Construction_Vertices_G4" test5,
                                     TestLabel "Construction_Vertices_G5" test6,
                                     TestLabel "Construction_Vertices_G6" test7,
                                     TestLabel "Construction_Vertices_G7" test8,
                                     TestLabel "Construction_Vertices_G8" test9,
                                     TestLabel "Construction_Vertices_G9" test10,
                                     TestLabel "Construction_Vertices_G10" test11,
                                     TestLabel "Construction_Vertices_G11" test12,
                                     TestLabel "Construction_Vertices_G12" test13,
                                     TestLabel "Construction_Vertices_G13" test14,
                                     TestLabel "Construction_Vertices_G14" test15,
                                     TestLabel "Construction_Vertices_G16" test16,
                                     TestLabel "Construction_Edges_G0_V1" test17,
                                     TestLabel "Construction_Edges_G1_V1" test18,
                                     TestLabel "Construction_Edges_G2_V1" test19,
                                     TestLabel "Construction_Edges_G2_V5" test20,
                                     TestLabel "Construction_Edges_G3_V1" test21,
                                     TestLabel "Construction_Edges_G3_V5" test22,
                                     TestLabel "Construction_Edges_G4_V1" test23,
                                     TestLabel "Construction_Edges_G4_V5" test24,
                                     TestLabel "Construction_Edges_G4_V12" test25,
                                     TestLabel "Construction_Edges_G5_V1" test26,
                                     TestLabel "Construction_Edges_G5_V5" test27,
                                     TestLabel "Construction_Edges_G5_V12" test28,
                                     TestLabel "Construction_Edges_G6_V1" test29,
                                     TestLabel "Construction_Edges_G6_V5" test30,
                                     TestLabel "Construction_Edges_G6_V12" test31,
                                     TestLabel "Construction_Edges_G7_V1" test32,
                                     TestLabel "Construction_Edges_G7_V5" test33,
                                     TestLabel "Construction_Edges_G7_V12" test34,
                                     TestLabel "Construction_Edges_G7_V7" test35,
                                     TestLabel "Construction_Edges_G8_V1" test36,
                                     TestLabel "Construction_Edges_G8_V5" test37,
                                     TestLabel "Construction_Edges_G8_V12" test38,
                                     TestLabel "Construction_Edges_G8_V7" test39,
                                     TestLabel "Construction_Edges_G9_V1" test40,
                                     TestLabel "Construction_Edges_G9_V5" test41,
                                     TestLabel "Construction_Edges_G9_V12" test42,
                                     TestLabel "Construction_Edges_G9_V7" test43,
                                     TestLabel "Construction_Edges_G10_V1" test44,
                                     TestLabel "Construction_Edges_G10_V5" test45,
                                     TestLabel "Construction_Edges_G10_V12" test46,
                                     TestLabel "Construction_Edges_G10_V7" test47,
                                     TestLabel "Construction_Edges_G10_V15" test48,
                                     TestLabel "Construction_Edges_G11_V1" test49,
                                     TestLabel "Construction_Edges_G11_V5" test50,
                                     TestLabel "Construction_Edges_G11_V12" test51,
                                     TestLabel "Construction_Edges_G11_V7" test52,
                                     TestLabel "Construction_Edges_G11_V15" test53,
                                     TestLabel "Construction_Edges_G12_V1" test54,
                                     TestLabel "Construction_Edges_G12_V5" test55,
                                     TestLabel "Construction_Edges_G12_V12" test56,
                                     TestLabel "Construction_Edges_G12_V7" test57,
                                     TestLabel "Construction_Edges_G12_V15" test58,
                                     TestLabel "Construction_Edges_G12_V(-2)" test59,
                                     TestLabel "Construction_Edges_G13_V1" test60,
                                     TestLabel "Construction_Edges_G13_V5" test61,
                                     TestLabel "Construction_Edges_G13_V12" test62,
                                     TestLabel "Construction_Edges_G13_V7" test63,
                                     TestLabel "Construction_Edges_G13_V15" test64,
                                     TestLabel "Construction_Edges_G13_V(-2)" test65,
                                     TestLabel "Construction_Edges_G14_V1" test66,
                                     TestLabel "Construction_Edges_G14_V5" test67,
                                     TestLabel "Construction_Edges_G14_V12" test68,
                                     TestLabel "Construction_Edges_G14_V7" test69,
                                     TestLabel "Construction_Edges_G14_V15" test70,
                                     TestLabel "Construction_Edges_G14_V(-2)" test71,
                                     TestLabel "Construction_Edges_G15_V1" test72,
                                     TestLabel "Construction_Edges_G15_V5" test73,
                                     TestLabel "Construction_Edges_G15_V12" test74,
                                     TestLabel "Construction_Edges_G15_V7" test75,
                                     TestLabel "Construction_Edges_G15_V15" test76,
                                     TestLabel "Construction_Edges_G15_V(-2)" test77,
                                     TestLabel "Idempotence_Vertices" test78,
                                     TestLabel "Idempotence_Edges" test79,
                                     TestLabel "Edges_Invalid_Head" test80,
                                     TestLabel "Edges_Invalid_Tail" test81,
                                     TestLabel "Cycle_Extend_0" test82,
                                     TestLabel "Cycle_Extend_1" test83,
                                     TestLabel "Cycle_Extend_2" test84,
                                     TestLabel "Cycle_Extend_3" test85,
                                     TestLabel "Cycle_Extend_4" test86,
                                     TestLabel "Cycle_Extend_5" test87,
                                     TestLabel "Cycle_Extend_6" test88,
                                     TestLabel "CycleDetection_V12" test89,
                                     TestLabel "CycleDetection_V(-2)" test90,
                                     TestLabel "CycleDetection_V7" test91,
                                     TestLabel "CycleDetection_V5" test92,
                                     TestLabel "CycleDetection_V15" test93,
                                     TestLabel "CycleDetection_V1" test94,
                                     TestLabel "CycleDetection_V100" test95,
                                     TestLabel "CycleDetection_V200" test96,
                                     TestLabel "CycleDetection_Invalid" test97,
                                     TestLabel "CycleDetection_ListWCycle" test98,
                                     TestLabel "CycleDetection_ListWOCycle" test99,
                                     TestLabel "CycleDetection_GraphWCycle" test100,
                                     TestLabel "CycleDetection_GraphWOCycle" test101,
                                     TestLabel "FoldPath" test102,
                                     TestLabel "MapGraph_Id_1" test103,
                                     TestLabel "MapGraph_Id_2" test104,
                                     TestLabel "MapGraph_Iso_1" test105,
                                     TestLabel "MapGraph_Iso_2" test106,
                                     TestLabel "MapGraph_Hom" test107,
                                     TestLabel "FindCone_1" test108,
                                     TestLabel "FindCone_2" test109,
                                     TestLabel "FindCone_3" test110,
                                     TestLabel "FindCone_4" test111,
                                     TestLabel "FindCone_4" test112]

main = defaultMain tests
