module Tibet
       ( start
       ) where


import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, readMVar)
import Control.DeepSeq (deepseq)
import Control.Monad (forever)
import Control.Monad.Reader
import Data.Either (fromRight)
import Data.IntMap.Strict (IntMap)
import Data.Maybe (catMaybes)
import Data.RadixTree (RadixTree)
import Data.Text (Text)
import Path (fromAbsFile, parseAbsDir)
import Path.IO (listDir)
import Paths_tibet (getDataFileName)
import System.Exit (exitSuccess)
import System.IO (stdout)

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Error as ME

import Handlers (Dictionary, DictionaryMeta (..), makeTextMap, mergeWithNum, searchTranslation,
                 selectDict, separator, sortOutput, toDictionaryMeta)
import Labels (labels)
import Parse (WylieTibet, makeTibet, makeWylieTibet, parseWylieInput, radixTreeMaker)
import Prettify (blue, cyan, green, nothingFound, putTextFlush, red)


data Env = Env
  { envDictionaryMeta :: ![DictionaryMeta]
  , envWylieTibet     :: !WylieTibet
  , envRadixTree      :: !RadixTree
  , envHistory        :: !(MVar History)
  }

data History = History
    { currentPosition :: Maybe Int
    , queries         :: IntMap Text
    } deriving Show

start :: Maybe [Int] -> IO ()
start mSelectedId = do
    sylsPath <- getDataFileName "stuff/tibetan-syllables"
    syls <- T.decodeUtf8 <$> BS.readFile sylsPath
    ls <- labels
    dir <- getDataFileName "dicts/"
    dirAbs <- parseAbsDir dir
    files <- map fromAbsFile . snd <$> listDir dirAbs
    result <- traverse (\fp -> toDictionaryMeta ls fp <$> toDictionary fp) files
    let dmList = selectDict mSelectedId result
    historyEmpty <- newEmptyMVar
    let wt = makeWylieTibet syls
    let radix = radixTreeMaker syls
    let env = Env
            { envDictionaryMeta = dmList
            , envWylieTibet = wt
            , envRadixTree = radix
            , envHistory = historyEmpty
            }
    dmList `deepseq` runReaderT translator env

-- | Translator works forever until quit command
translator :: ReaderT Env IO ()
translator = ReaderT $ \env -> forever $ do
    putTextFlush $ blue "Which a tibetan word to translate?"
    query <- fmap (T.strip . T.decodeUtf8) $ BS.hPutStr stdout "> " >> BS.getLine
    case query of
        ":q" -> do
            putTextFlush $ green "Bye-bye!"
            exitSuccess
        ":h" -> do
            history' <- IntMap.elems . queries <$> readMVar (envHistory env)
            if null history' then putTextFlush $ red "No success queries."
            else do
                putTextFlush $ green "Success queries:"
                mapM_ (\h -> T.putStrLn $ "- " <> h) history'
        _    -> do
            dscValues <- traverse (pure . searchTranslation query) (envDictionaryMeta env)
            let dictMeta = sortOutput $ catMaybes dscValues
            if null dscValues then nothingFound
            else do
                let toTibetan = makeTibet (envWylieTibet env) . parseWylieInput (envRadixTree env)
                case traverse (separator [37] toTibetan) dictMeta of
                    Left err -> putStrLn $ ME.errorBundlePretty err
                    Right list -> do
                        let translations = mergeWithNum list
                        let tibQuery = cyan . fromRight query $ T.concat <$> toTibetan query
                        T.putStrLn tibQuery
                        T.putStrLn translations
                        modifyHistory env $ pure . addQuery query

toDictionary :: FilePath -> IO Dictionary
toDictionary path = makeTextMap . T.decodeUtf8 <$> BS.readFile path

modifyHistory :: Env -> (History -> IO History) -> IO ()
modifyHistory = modifyMVar_ . envHistory

addQuery :: Text -> History -> History
addQuery query History{..} = History currentPosition newQueries
  where
    newKey = IntMap.size queries + 1
    newQueries = IntMap.insert newKey query queries
