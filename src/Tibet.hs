module Tibet
       ( start
       ) where

import           Control.DeepSeq (deepseq)
import           Control.Monad.Trans.State.Strict (execStateT, get, withStateT)
import           Data.ByteString.Char8 (ByteString)
import           Path (fromAbsFile)
import           Path.Internal (Path (..))
import           Path.IO (listDir)
import           System.IO (stderr)

import           Handlers (Dictionary, History, Title, mergeWithNum, searchInMap, zipWithMap)
import           Labels (labels)
import           Paths_tibet (getDataFileName)
import           Prettify (blue, green, putTextFlush, red)

import qualified Data.ByteString.Char8 as BC


start :: IO ()
start = do
    dir <- getDataFileName "dicts/"
    (_, files) <- listDir $ Path dir
    texts <- mapM (BC.readFile . fromAbsFile) files
    mapped <- zipWithMap texts files <$> labels
    let historyS = get
    mapped `deepseq` cli mapped historyS ""

cli :: [(Dictionary, (Title, Int))] -> History -> ByteString -> IO ()
cli mapped historyOldS _ = do
    putTextFlush $ blue "Which a tibetan word to translate?"
    query <- BC.hPutStr stderr "> " >> BC.getLine
    case query of
        ":q" -> putTextFlush $ green "Bye-bye!"
        ":h" -> do
            history <- execStateT historyOldS []
            if null history then do
                putTextFlush $ red "No success queries."
                putTextFlush ""
            else do
                putTextFlush $ green "Success queries:"
                mapM_ (\h -> BC.putStrLn $ "- " <> h) history
                putTextFlush ""
            cli mapped historyOldS ""
        _    -> do
            let dscValues = searchInMap query mapped
            if (null dscValues) then do
                putTextFlush $ red "Nothing found."
                putTextFlush ""
                cli mapped historyOldS ""
            else do
                let historyNewS = withStateT (query :) historyOldS
                putTextFlush $ mergeWithNum dscValues
                cli mapped historyNewS ""
