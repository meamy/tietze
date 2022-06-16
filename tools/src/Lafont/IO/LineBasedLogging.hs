-- | Helper methods to format logs originating from a single line in a single file.

module Lafont.IO.LineBasedLogging where

-----------------------------------------------------------------------------------------
-- * Logging By: File and Line Number.

-- | Consumes a file name (fname) and line number (n). Return a prefix for logs
-- originating from this line.
fileLogPrefix :: String -> Int -> String
fileLogPrefix fname n = "[" ++ fname ++ ":" ++ (show n) ++ "]"

-- | Consumes a file name (fname), line number (n), and message (msg). Returns a modified
-- copy of msg with (fileLogPrefix fname n) prepended.
logFromFile :: String -> Int -> String -> String
logFromFile fname n msg = (fileLogPrefix fname n) ++ " " ++ msg ++ "\n"

-- | Consumes a file name (fname), line number (n), and one of either two showable types.
-- If msg is the showable consumed, then returns (logFromFile fname n msg). For example,
-- logEitherMsg can be used to display exceptions of type (Either GeneralErr SpecificErr).
logEitherMsg :: (Show a, Show b) => String -> Int -> Either a b -> String
logEitherMsg fname n (Left msg)  = (logFromFile fname n (show msg))
logEitherMsg fname n (Right msg) = (logFromFile fname n (show msg))
