-- | Utilities to test the logic of an executable. Requires that the logic of the
-- executable has been factored out into a library, and that the entry-point of the
-- executable takes as input a handle to use as an output stream.

module LafontExeTest.HandleTest where

import System.Directory
import System.Exit
import System.IO

-- | Group together a test name, its description (printed on failure), a function under
-- test that produces output to a handle, and a function to validate all data written to
-- the handle. For proper formatting, both name and msg should be free from newline
-- characters.
data HandleTest = HandleTest { name :: String
                             , msg :: String
                             , testFn :: (Handle -> IO ())
                             , checkFn :: (String -> Bool)
                             }

-- | Consumes a HandleTest with testing function test and validation function check. The
-- test function is excuted using a temporary file as its handle. Returns true if and
-- only if the data written to the handle satisfies check.
--
-- Adapted from: https://stackoverflow.com/a/9664017
runHandleTest :: HandleTest -> IO Bool
runHandleTest htest = do
    -- Creates temporary file.
    tmpDir <- getTemporaryDirectory
    (tmpFn, hdl) <- openTempFile tmpDir "test_out.txt"
    -- Runs test.
    ((testFn htest) hdl)
    hClose hdl
    -- Processes result and cleanup.
    content <- readFile tmpFn
    let res = ((checkFn htest) content) in do
        removeFile tmpFn
        return res

-- | Consumes a list of handle tests. Runs each test using runHandleTest. For each test
-- that fails, its name and error message are displayed.
runAllHandleTests :: [HandleTest] -> IO Bool
runAllHandleTests []             = return True
runAllHandleTests (htest:htests) = do
    -- Runs test and prints results.
    res <- runHandleTest htest
    putStr ("[" ++ (name htest) ++ "]: ")
    if res
    then putStr ("Success.\n")
    else putStr ("Fail! (" ++ (msg htest) ++ ")\n")
    -- Accumulates results.
    acc <- runAllHandleTests htests
    return (res && acc)

-- | Helper function to convert the output of runAllHandleTests to an output code. As the
-- type would suggest, this function also works with runHandleTest when there is a single
-- test trial in the suite.
handleTestToMain :: IO Bool -> IO Bool
handleTestToMain io = do
    res <- io
    if res
    then exitWith ExitSuccess
    else exitWith (ExitFailure 1)
