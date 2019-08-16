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
import Paths_tibet (getDataFileName)
import System.Console.Haskeline (defaultSettings, getHistory, getInputLine)
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.IO
import System.Exit (exitSuccess)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Error as ME

import Handlers (Dictionary, DictionaryMeta (..), makeTextMap, mergeWithNum, searchTranslation,
                 selectDict, separator, sortOutput, toDictionaryMeta)
import Labels (labels)
import Parse (WylieTibet, makeTibet, makeWylieTibet, parseWylieInput, radixTreeMaker)
import Prettify (blue, cyan, green, nothingFound, putTextFlush)


-- | Environment fot translator
data Env = Env
  { envDictionaryMeta :: ![DictionaryMeta]
  , envWylieTibet     :: !WylieTibet
  , envRadixTree      :: !RadixTree
  }

-- | Load all stuff for environment.
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
    let wt = makeWylieTibet syls
    let radix = radixTreeMaker syls
    let env = Env
            { envDictionaryMeta = dmList
            , envWylieTibet = wt
            , envRadixTree = radix
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
        putTextFlush $ blue "Which a tibetan word to translate?"
        mQuery <- queryInput inputState $ getInputLine "> "
        case T.strip . T.pack <$> mQuery of
            Nothing -> return ()
            Just ":q" -> do
                putTextFlush $ green "Bye-bye!"
                exitSuccess
            Just ":h" -> do
                history <- queryInput inputState getHistory
                mapM_ (T.putStrLn . T.pack) $ reverse $ historyLines history
            Just query -> do
                dscMaybeValues <- traverse (pure . searchTranslation query) (envDictionaryMeta env)
                let dscValues = catMaybes dscMaybeValues
                if null dscValues then nothingFound
                else do
                    let dictMeta = sortOutput dscValues
                    let toTibetan = makeTibet (envWylieTibet env) . parseWylieInput (envRadixTree env)
                    case traverse (separator [37] toTibetan) dictMeta of
                        Left err -> putStrLn $ ME.errorBundlePretty err
                        Right list -> do
                            let translations = mergeWithNum list
                            case toTibetan query of
                                Left err  -> putStrLn $ ME.errorBundlePretty err
                                Right tib -> T.putStrLn $ cyan $ T.concat tib
                            T.putStrLn translations

toDictionary :: FilePath -> IO Dictionary
toDictionary path = makeTextMap . T.decodeUtf8 <$> BS.readFile path
