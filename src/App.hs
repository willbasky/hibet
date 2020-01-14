module App
  ( start
  )
  where


import Control.DeepSeq (deepseq)
import Control.Exception (bracketOnError)
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Data.List (foldl')
import Data.Text (Text)
import Path (fromAbsFile, parseAbsDir)
import Path.IO (listDir)
import Paths_Hibet (getDataFileName)
import System.Console.Haskeline (defaultSettings, getHistory, getInputLine)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Megaparsec.Error as ME

import Interpretator
import Labels (labels)
import Language
import Parse
import Pretty
import Translate (Dictionary, getAnswer, makeTextMap, selectDict, toDictionaryMeta)
import Types


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
    dmList `deepseq` runReaderT app env

-- | Translator works forever until quit command.
app :: ReaderT Env IO ()
app = ReaderT $ \env ->
    bracketOnError (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such as a SigINT is received.
        (\inputState -> loop env inputState >> closeInput inputState)
  where
    loop :: Env -> InputState -> IO ()
    loop env inputState = forever $ runHibet $ do
        putColorTextH blue "Which a tibetan word to translate?"
        mQuery <- queryInputH inputState $ getInputLine "> "
        case T.strip . T.pack <$> mQuery of
            Nothing -> pure ()
            Just ":q" -> do
                putColorTextH yellow "Bye-bye!"
                exitH
            Just ":h" -> do
                history <- fromHistory <$> queryInputH inputState getHistory
                mapM_ (putColorTextH id) history
            Just query -> do
                let answerE = runExcept $ getAnswer query env
                case answerE of
                  Left err -> putColorTextH red $ T.pack $ ME.errorBundlePretty err
                  Right (answer, isEmpty) -> do
                      when isEmpty $ putColorTextH red "Nothing found"
                      pprintH answer


toDictionary :: FilePath -> IO Dictionary
toDictionary path = makeTextMap . T.decodeUtf8 <$> BS.readFile path

fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines
