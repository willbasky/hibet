{-# LANGUAGE TemplateHaskell #-}

module Tibet
       ( start
       ) where

import           Handlers (zipWithRaw, directSearch, mergeWithNum)

import           Path (fromAbsFile, mkRelDir)
import           Path.IO (listDir)
import           System.IO (stderr)

import qualified Data.Text.IO as IO


start :: IO ()
start = do
    IO.hPutStrLn stderr "What tibetan word to translate?"
    cli

cli :: IO ()
cli = do
    query <- IO.hPutStr stderr "> " >> IO.getLine
    case query of
        ":q" -> IO.hPutStrLn stderr "Bye-bye"
        _    -> do
            (_, files) <- listDir $(mkRelDir "./dics/")
            texts <- mapM (IO.readFile . fromAbsFile) files
            let zipped = zipWithRaw texts files
            let dscValues = directSearch query zipped
            if null dscValues then putStrLn "Nothing found"
            else IO.hPutStrLn stderr $ mergeWithNum dscValues
            start




