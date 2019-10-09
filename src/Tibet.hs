module Tibet
       ( start
       ) where


import Control.DeepSeq (deepseq)
import Control.Exception (bracketOnError)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Data.Maybe (catMaybes)
import Data.RadixTree (RadixTree)
import Path (fromAbsFile, parseAbsDir)
import Path.IO (listDir)
import Paths_Hibet (getDataFileName)
import System.Console.Haskeline (defaultSettings, getHistory, getInputLine)
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.IO
import System.Exit (exitSuccess)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Error as ME

import Handlers (Dictionary, DictionaryMeta (..), makeTextMap, searchTranslation, selectDict,
                 separator, sortOutput, toDictionaryMeta)
import Labels (labels)
import Parse
import Pretty


-- | Environment fot translator
data Env = Env
  { envDictionaryMeta :: ![DictionaryMeta]
  , envWylieTibet     :: !WylieTibet
  , envTibetWylie     :: !TibetWylie
  , envRadixWylie     :: !RadixTree
  , envRadixTibet     :: !RadixTree
  }

-- | Load all stuff for environment.
start :: [Int] -> IO ()
start selectedIds = do
    sylsPath <- getDataFileName "stuff/tibetan-syllables"
    syls <- T.decodeUtf8 <$> BS.readFile sylsPath
    ls <- labels
    dir <- getDataFileName "dicts/"
    dirAbs <- parseAbsDir dir
    files <- map fromAbsFile . snd <$> listDir dirAbs
    result <- traverse (\fp -> toDictionaryMeta ls fp <$> toDictionary fp) files
    let dmList = selectDict selectedIds result
    let wt = makeWylieTibet syls
    let tw = makeTibetWylie syls
    let radixWylie = makeWylieRadexTree syls
    let radixTibet = makeTibetanRadexTree syls
    let env = Env
            { envDictionaryMeta = dmList
            , envWylieTibet = wt
            , envTibetWylie = tw
            , envRadixWylie = radixWylie
            , envRadixTibet = radixTibet
            }
    dmList `deepseq` runReaderT translator env

-- | Translator works forever until quit command.
translator :: ReaderT Env IO ()
translator = ReaderT $ \env ->
    bracketOnError (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such as a SigINT is received.
        (\inputState -> loop env inputState >> closeInput inputState)
  where
    loop :: Env -> InputState -> IO ()
    loop env inputState = forever $ do
        putColorDoc blue "Which a tibetan word to translate?"
        mQuery <- queryInput inputState $ getInputLine "> "
        case T.strip . T.pack <$> mQuery of
            Nothing -> return ()
            Just ":q" -> do
                putColorDoc blue "Bye-bye!"
                exitSuccess
            Just ":h" -> do
                history <- queryInput inputState getHistory
                mapM_ (T.putStrLn . T.pack) $ reverse $ historyLines history
            Just query -> do
                let toWylie' = toWylie (envTibetWylie env) . parseTibetanInput (envRadixTibet env)
                let wylieQuery = case toWylie' query  of
                        Left _      -> query
                        Right wylie -> if T.null wylie then query else wylie
                dscMaybeValues <- traverse (pure . searchTranslation wylieQuery) (envDictionaryMeta env)
                let dscValues = catMaybes dscMaybeValues
                if null dscValues then putColorDoc red "Nothing found."
                else do
                    let dictMeta = sortOutput dscValues
                    let toTibetan' = toTibetan (envWylieTibet env) . parseWylieInput (envRadixWylie env)
                    case traverse (separator [37] toTibetan') dictMeta of
                        Left err -> putStrLn $ ME.errorBundlePretty err
                        Right list -> do
                            let translations = viewTranslations list
                            let eitherQuery = if query == wylieQuery
                                    then T.concat <$> toTibetan' wylieQuery
                                    else Right wylieQuery
                            case eitherQuery of
                                Left err     -> putStrLn $ ME.errorBundlePretty err
                                Right query' -> pprint $ withHeaderSpaces yellow query' translations

toDictionary :: FilePath -> IO Dictionary
toDictionary path = makeTextMap . T.decodeUtf8 <$> BS.readFile path
