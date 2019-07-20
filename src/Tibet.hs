module Tibet
       ( start
       ) where

import Conduit (decodeUtf8C, foldMapC, runConduitRes, sourceFileBS, (.|))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.DeepSeq (deepseq)
import Control.Monad (forever)
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.RadixTree (RadixTree)
import Data.Text (Text)
import Path (fromAbsFile)
import Path.Internal (Path (..))
import Path.IO (listDir)
import Paths_tibet (getDataFileName)
import System.Exit
import System.IO (stdout)

import Handlers (Dictionary, DictionaryMeta (..), makeTextMap, mergeWithNum, searchTranslation,
                 selectDict, separator, sortOutput, toDictionaryMeta)
import Labels (labels)
import Parse (WylieTibet, makeTibet, makeWylieTibet, parseWylieInput, radixTreeMaker)
import Prettify (blue, cyan, green, nothingFound, putTextFlush, red)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Error as ME


start :: Maybe [Int] -> IO ()
start mSelectedId = do
    sylsPath <- getDataFileName "parser/tibetan-syllables"
    syls <- T.decodeUtf8 <$> BC.readFile sylsPath
    ls <- labels
    dir <- getDataFileName "dicts/"
    files <- map fromAbsFile . snd <$> listDir (Path dir)
    result <- mapConcurrently (\fp -> toDictionaryMeta ls fp <$> toDictionaryC fp) files
    let mapped = selectDict mSelectedId result
    history <- newMVar []
    let wt = makeWylieTibet syls
    let radixTree = radixTreeMaker syls
    mapped `deepseq` translator mapped wt radixTree history

-- | A loop handler of user commands.
translator :: [DictionaryMeta] -> WylieTibet -> RadixTree -> MVar [Text] -> IO ()
translator mapped wt radixTree history = forever $ do
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
            dscValues <- mapConcurrently (pure . searchTranslation query) mapped
            let dictMeta = sortOutput $ catMaybes dscValues
            if null dscValues then nothingFound
            else do
                let toTibetan = makeTibet wt . parseWylieInput radixTree
                case traverse (separator [37] toTibetan) dictMeta of
                    Left err -> putStrLn $ ME.errorBundlePretty err
                    Right list -> do
                        let translations = mergeWithNum list
                        let tibQuery = cyan . fromRight query $ T.concat <$> toTibetan query
                        T.putStrLn tibQuery
                        T.putStrLn translations
                        modifyMVar_ history (pure . (query :))

toDictionaryC :: FilePath -> IO Dictionary
toDictionaryC path = runConduitRes
    $ sourceFileBS path
    .| decodeUtf8C
    .| foldMapC makeTextMap
