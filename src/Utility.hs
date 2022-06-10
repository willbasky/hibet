module Utility
  (
    showT
  , filename
  , mkAbsolute
  , T.pack
  , debugEnabledEnvVar
  ,toTitle) where

import Data.Char (toLower, toUpper)
import Data.List.Extra (takeWhileEnd)
import Data.Map as Map (Map, fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (makeAbsolute)
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

environmentVars :: IO (Map String String)
environmentVars = Map.fromList <$> getEnvironment

lookupEnv :: String -> IO (Maybe String)
lookupEnv k = Map.lookup k <$> environmentVars

debugEnabledEnvVar :: IO Bool
debugEnabledEnvVar = do
  isDebug <- lookupEnv "HIBET_DEBUG"
  pure $ Just True == (readMaybe . toTitle =<< isDebug)

toTitle :: String -> String
toTitle ""     = ""
toTitle (x:xs) = toUpper x : map toLower xs

showT :: Show a => a -> Text
showT = T.pack . show

filename :: FilePath -> FilePath
filename = takeWhileEnd (/= '/')

mkAbsolute :: FilePath -> FilePath
mkAbsolute = unsafePerformIO . makeAbsolute
