{-# LANGUAGE TemplateHaskell #-}

module Tibet
       ( start
       ) where

import           Control.DeepSeq (deepseq)
import           Control.Monad (when)
import           Path (fromAbsFile, mkRelDir)
import           Path.IO (listDir)
import           System.IO (stderr)

import           Handlers (Dictionary, Title, mergeWithNum, searchInMap, zipWithMap)
import           Prettify (blueCode, greenCode, putTextFlush, redCode, resetCode)

import qualified Data.Text.IO as IO


start :: IO ()
start = do
    (_, files) <- listDir $(mkRelDir "./dics/")
    texts <- mapM (IO.readFile . fromAbsFile) files
    let mapped = zipWithMap texts files
    mapped `deepseq` cli mapped

cli :: [(Dictionary, Title)] -> IO ()
cli mapped = do
    putTextFlush $ blueCode <> "Which a tibetan word to translate?" <> resetCode
    query <- IO.hPutStr stderr "> " >> IO.getLine
    case query of
        ":q" -> putTextFlush $ greenCode <> "Bye-bye!" <> resetCode
        _    -> do
            let dscValues = searchInMap query mapped
            when (null dscValues) $ putTextFlush $ redCode <> "Nothing found." <> resetCode
            putTextFlush $ mergeWithNum dscValues
            cli mapped
