module Tibet
       ( start
       ) where

import           Control.DeepSeq (deepseq)
import           Control.Monad (when)
import           Path (fromAbsFile)
import           Path.Internal (Path(..))
import           Path.IO (listDir)
import           System.IO (stderr)

import           Handlers (Dictionary, Title, mergeWithNum, searchInMap, zipWithMap)
import           Paths_tibet (getDataFileName)
import           Prettify (blueCode, greenCode, putTextFlush, redCode, resetCode)

import qualified Data.ByteString.Char8 as BC


start :: IO ()
start = do
    dir <- getDataFileName "dicts/"
    (_, files) <- listDir $ Path dir
    texts <- mapM (BC.readFile . fromAbsFile) files
    let mapped = zipWithMap texts files
    mapped `deepseq` cli mapped

cli :: [(Dictionary, Title)] -> IO ()
cli mapped = do
    putTextFlush $ blueCode <> "Which a tibetan word to translate?" <> resetCode
    query <- BC.hPutStr stderr "> " >> BC.getLine
    case query of
        ":q" -> putTextFlush $ greenCode <> "Bye-bye!" <> resetCode
        _    -> do
            let dscValues = searchInMap query mapped
            when (null dscValues) $ putTextFlush $ redCode <> "Nothing found." <> resetCode
            putTextFlush $ mergeWithNum dscValues
            cli mapped
