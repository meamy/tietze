module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Either

-----------------------------------------------------------------------------------------
-- updateRight and updateLeft

lval1 :: Either Int String
lval1 = Left 5

rval1 :: Either Int String
rval1 = Right "Hello"

f :: Int -> [Int]
f n = [n, n + 1]

g :: String -> [String]
g s = [s, s ++ s]

test1 = TestCase (assertEqual "Can update on left given a left value."
                              (Left [5,6] :: Either [Int] String)
                              (updateLeft lval1 f))

test2 = TestCase (assertEqual "Can update on left given a right value."
                              (Right "Hello" :: Either [Int] String)
                              (updateLeft rval1 f))

test3 = TestCase (assertEqual "Can update on right given a left value."
                              (Left 5 :: Either Int [String])
                              (updateRight lval1 g))

test4 = TestCase (assertEqual "Can update on right given a right value."
                              (Right ["Hello", "HelloHello"] :: Either Int [String])
                              (updateRight rval1 g))

-----------------------------------------------------------------------------------------
-- branchRight and branchLeft

lval2 :: Either Int String
lval2 = Left 6

rval2 :: Either Int String
rval2 = Right "6"

h :: Int -> Either [Int] String
h 6 = Right "6"
h n = Left (f n)

k :: String -> Either Int [String]
k "6" = Left 6
k str = Right (g str)

test5 = TestCase (assertEqual "Can branch on left given a left value (to left)."
                              (Left [5,6] :: Either [Int] String)
                              (branchLeft lval1 h))

test6 = TestCase (assertEqual "Can branch on left given a left value (to right)."
                              (Right "6" :: Either [Int] String)
                              (branchLeft lval2 h))

test7 = TestCase (assertEqual "Can branch on left given a right value."
                              (Right "Hello" :: Either [Int] String)
                              (branchLeft rval1 h))

test8 = TestCase (assertEqual "Can branch on right given a left value."
                              (Left 5 :: Either Int [String])
                              (branchRight lval1 k))

test9 = TestCase (assertEqual "Can branch on right given a right value (to right)."
                              (Right ["Hello", "HelloHello"] :: Either Int [String])
                              (branchRight rval1 k))

test10 = TestCase (assertEqual "Can branch on right given a right value (to left)."
                               (Left 6 :: Either Int [String])
                               (branchRight rval2 k))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "updateLeft_Left" test1,
                                     TestLabel "updateLeft_Right" test2,
                                     TestLabel "updateRight_Left" test3,
                                     TestLabel "updateRight_Right" test4,
                                     TestLabel "branchLeft_Left_Left" test5,
                                     TestLabel "branchLeft_Left_Right" test6,
                                     TestLabel "branchLeft_Right" test7,
                                     TestLabel "branchRight_Left" test8,
                                     TestLabel "branchRight_Right_Right" test9,
                                     TestLabel "branchRight_Right_Left" test10]

main = defaultMain tests
