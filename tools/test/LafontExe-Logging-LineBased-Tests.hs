module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import LafontExe.Logging.LineBased

-----------------------------------------------------------------------------------------
-- fileLogPrefix

test1 = TestCase (assertEqual "fileLogPrefix (1/3)."
                              "[abcd:10]"
                              (fileLogPrefix "abcd" 10))

test2 = TestCase (assertEqual "fileLogPrefix (2/3)."
                              "[/home/user/dir/file.txt:10]"
                              (fileLogPrefix "/home/user/dir/file.txt" 10))

test3 = TestCase (assertEqual "fileLogPrefix (3/3)."
                              "[abcd:425]"
                              (fileLogPrefix "abcd" 425))

-----------------------------------------------------------------------------------------
-- logFromFile

test4 = TestCase (assertEqual "logFromFile (1/4)."
                              "[abcd:10] some text.\n"
                              (logFromFile "abcd" 10 "some text."))

test5 = TestCase (assertEqual "logFromFile (2/4)."
                              "[/home/user/dir/file.txt:10] some text.\n"
                              (logFromFile "/home/user/dir/file.txt" 10 "some text."))

test6 = TestCase (assertEqual "logFromFile (3/4)."
                              "[abcd:425] some text.\n"
                              (logFromFile "abcd" 425 "some text."))

test7 = TestCase (assertEqual "logFromFile (4/4)."
                              "[abcd:10] alternative words!\n"
                              (logFromFile "abcd" 10 "alternative words!"))

-----------------------------------------------------------------------------------------
-- logEitherMsg

instance Display Int where
    display n = show n

lval1 :: Either Int Int
lval1 = Left 5

lval2 :: Either Int Int
lval2 = Left 6

test8 = TestCase (assertEqual "logFromFile printing left (1/4)."
                              "[abcd:10] 5\n"
                              (logEitherMsg "abcd" 10 lval1))

test9 = TestCase (assertEqual "logFromFile printing left (2/4)."
                              "[/home/user/dir/file.txt:10] 5\n"
                              (logEitherMsg "/home/user/dir/file.txt" 10 lval1))

test10 = TestCase (assertEqual "logFromFile printing left (3/4)."
                               "[abcd:425] 5\n"
                               (logEitherMsg "abcd" 425 lval1))

test11 = TestCase (assertEqual "logFromFile printing left (4/4)."
                               "[abcd:10] 6\n"
                               (logEitherMsg "abcd" 10 lval2))

rval1 :: Either Int Int
rval1 = Right 10

rval2 :: Either Int Int
rval2 = Right 11

test12 = TestCase (assertEqual "logFromFile printing right (1/4)."
                               "[abcd:10] 10\n"
                               (logEitherMsg "abcd" 10 rval1))

test13 = TestCase (assertEqual "logFromFile printing right (2/4)."
                               "[/home/user/dir/file.txt:10] 10\n"
                               (logEitherMsg "/home/user/dir/file.txt" 10 rval1))

test14 = TestCase (assertEqual "logFromFile printing right (3/4)."
                               "[abcd:425] 10\n"
                               (logEitherMsg "abcd" 425 rval1))

test15 = TestCase (assertEqual "logFromFile printing right (4/4)."
                               "[abcd:10] 11\n"
                               (logEitherMsg "abcd" 10 rval2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "fileLogPrefix_TestOne" test1,
                                     TestLabel "fileLogPrefix_TestTwo" test2,
                                     TestLabel "fileLogPrefix_TestThree" test3,
                                     TestLabel "logFromFile_TestOne" test4,
                                     TestLabel "logFromFile_TestTwo" test5,
                                     TestLabel "logFromFile_TestThree" test6,
                                     TestLabel "logFromFile_TestFour" test7,
                                     TestLabel "logFromFile_LeftTestOne" test8,
                                     TestLabel "logFromFile_LeftTestTwo" test9,
                                     TestLabel "logFromFile_LeftTestThree" test10,
                                     TestLabel "logFromFile_LeftTestFour" test11,
                                     TestLabel "logFromFile_RightTestOne" test12,
                                     TestLabel "logFromFile_RightTestTwo" test13,
                                     TestLabel "logFromFile_RightTestThree" test14,
                                     TestLabel "logFromFile_RightTestFour" test15]

main = defaultMain tests
