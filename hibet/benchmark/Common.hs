module Common where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Data root: @HIBET_DATA_DIR@ or @../data@ in a repository checkout.
dataDir :: IO FilePath
dataDir = do
    envRoot <- lookupEnv "HIBET_DATA_DIR"
    pure $ maybe "../data" id envRoot

dictPaths :: IO [FilePath]
dictPaths = do
    dir <- (</> "dicts") <$> dataDir
    map (dir </>) <$> listDirectory dir

textLoad :: IO [Text]
textLoad = dictPaths >>= mapM (fmap TE.decodeUtf8 . BS.readFile)
