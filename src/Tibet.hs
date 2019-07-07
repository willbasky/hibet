module Tibet
       ( start
       ) where

import Control.DeepSeq (deepseq)
import Control.Monad (forever, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT, get, put, withStateT)
import Data.Text (Text)
import Path (fromAbsFile)
import Path.Internal (Path (..))
import Path.IO (listDir)
import Paths_tibet (getDataFileName)
import System.Exit
import System.IO (stdout)

import Handlers (DictionaryMeta, History, mergeWithNum, searchInMap, selectDict, separator,
                 zipWithMap)
import Labels (labels)
import Parse (toTibet)
import Prettify (blue, cyan, green, putTextFlush, red)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T


-- | Iterator with state holding.
iterateM :: Monad m => (History -> m History) -> History -> m ()
iterateM f = evalStateT $ forever $ get >>= lift . f >>= put

start :: Maybe [Int] -> IO ()
start mSelectedId = do
    syls <- T.readFile "./parser/tibetan-syllables"
    dir <- getDataFileName "dicts/"
    (_, files) <- listDir $ Path dir
    texts <- mapM (fmap T.decodeUtf8 . BC.readFile . fromAbsFile) files
    mappedFull <- zipWithMap texts files <$> labels
    let mapped = selectDict mSelectedId mappedFull
    let history = get
    mapped `deepseq` translator mapped syls history

-- | A loop handler of user commands.
translator :: [DictionaryMeta] -> Text -> History -> IO ()
translator mapped syls = iterateM $ \history -> do
    putTextFlush $ blue "Which a tibetan word to translate?"
    query <- fmap (T.strip . T.decodeUtf8) $ BC.hPutStr stdout "> " >> BC.getLine
    case query of
        ":q" -> do
            putTextFlush $ green "Bye-bye!"
            exitSuccess
        ":h" -> do
            history' <- execStateT history []
            when (null history') (putTextFlush $ red "No success queries.")
            putTextFlush $ green "Success queries:"
            mapM_ (\h -> T.putStrLn $ "- " <> h) history'
            putTextFlush ""
            pure history
        _    -> do
            let dscValues = searchInMap query mapped
            if null dscValues then do
                nothingFound
                pure history
            else do
                let translations = mergeWithNum $ map (separator [37] syls) dscValues
                let tibQuery = cyan $ T.concat $ toTibet syls query
                T.putStrLn tibQuery
                T.putStrLn translations
                pure $ withStateT (query :) history

nothingFound :: IO ()
nothingFound = do
    putTextFlush $ red "Nothing found."
    putTextFlush ""
