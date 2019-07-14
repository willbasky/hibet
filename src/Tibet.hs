module Tibet
       ( start
       ) where

import Control.DeepSeq (deepseq)
import Control.Monad (forever)
import Data.Either (fromRight)
import Data.Text (Text)
import Path (fromAbsFile)
import Path.Internal (Path (..))
import Path.IO (listDir)
import Paths_tibet (getDataFileName)
import System.Exit
import System.IO (stdout)
import Control.Concurrent.MVar

import Handlers (DictionaryMeta, mergeWithNum, searchInMap, selectDict, separator,
                 zipWithMap)
import Labels (labels)
import Parse (toTibet)
import Prettify (blue, cyan, green, putTextFlush, red)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Error as ME


start :: Maybe [Int] -> IO ()
start mSelectedId = do
    sylsPath <- getDataFileName "parser/tibetan-syllables"
    syls <- T.readFile sylsPath
    dir <- getDataFileName "dicts/"
    (_, files) <- listDir $ Path dir
    texts <- mapM (fmap T.decodeUtf8 . BC.readFile . fromAbsFile) files
    mappedFull <- zipWithMap texts files <$> labels
    let mapped = selectDict mSelectedId mappedFull
    history <- newMVar []
    mapped `deepseq` translator mapped syls history

-- | A loop handler of user commands.
translator :: [DictionaryMeta] -> Text -> MVar [Text] -> IO ()
translator mapped syls history = forever $ do
    putTextFlush $ blue "Which a tibetan word to translate?"
    query <- fmap (T.strip . T.decodeUtf8) $ BC.hPutStr stdout "> " >> BC.getLine
    case query of
        ":q" -> do
            putTextFlush $ green "Bye-bye!"
            exitSuccess
        ":h" -> do
            history' <- readMVar history
            if null history' then putTextFlush $ red "No success queries."
            else do
                putTextFlush $ green "Success queries:"
                mapM_ (\h -> T.putStrLn $ "- " <> h) history'
        _    -> do
            let dscValues = searchInMap query mapped
            if null dscValues then nothingFound
            else
                case traverse (separator [37] syls) dscValues of
                    Left err -> putStrLn $ ME.errorBundlePretty err
                    Right list -> do
                        let translations = mergeWithNum list
                        let tibQuery = cyan . fromRight query $ T.concat <$> toTibet syls query
                        T.putStrLn tibQuery
                        T.putStrLn translations
                        modifyMVar_ history (pure . (query :))

nothingFound :: IO ()
nothingFound = do
    putTextFlush $ red "Nothing found."
    putTextFlush ""
