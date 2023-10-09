{-# LANGUAGE DeriveGeneric #-}

-- | Helper functions to interace with yaml configurations.

module LafontExe.IO.Configs where

import           Data.Yaml
import           GHC.Generics

-----------------------------------------------------------------------------------------
-- * Representation of Configurations.

-- | Data type to store the contents of a configuration file
data Config = Config { generators  :: String
                     , relations   :: String
                     , derivations :: [String]
                     , obligations :: [String]
                     } deriving (Generic,Show,Eq)
instance FromJSON Config

-- | Configuration parsing error wrapper.
data ConfigErr = YamlErr ParseException

-- | Utility to pretty print a configuration parsing error.
printConfigErr :: ConfigErr -> String
printConfigErr (YamlErr err) = prettyPrintParseException err

-----------------------------------------------------------------------------------------
-- * Parsing Configurations.

-- | Takes as input the name of a configuration file. If the file is valid, then returns
-- a summary of the configurations. Otherwise, nothing is returned.
parseConfigYamlImpl :: String -> IO (Either ConfigErr Config)
parseConfigYamlImpl fname = do
    res <- decodeFileEither fname
    case res of
        Left err   -> return $ Left $ YamlErr err
        Right conf -> return $ Right conf
