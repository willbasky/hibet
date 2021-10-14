-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TemplateHaskell  #-}



module Effects.File where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Paths_hibet (getDataFileName)
import Polysemy (Embed, Member, Sem)
import Polysemy.Error (Error, mapError, runError, throw)
import qualified Polysemy as P
import Polysemy.Path (Abs, File, Path, Dir, PathException, parseAbsDir)
import Path.IO (listDir)


data FileIO m a where
  ReadFile :: FilePath -> FileIO m BS.ByteString
  ReadFileLazy :: FilePath -> FileIO m BSL.ByteString
  GetPath :: FilePath -> FileIO m FilePath
  ListDirectory :: Path b Dir -> FileIO m ([Path Abs Dir], [Path Abs File])
  ParseAbsDirectory :: FilePath -> FileIO r (Either PathException (Path Abs Dir))

P.makeSem ''FileIO

-- readFile :: Member File r => FilePath -> Sem r BS.ByteString
-- readFile x = P.send (ReadFile x :: File (Sem r) BS.ByteString)

runFile :: Member (Embed IO) r => Sem (FileIO : r) a -> Sem r a
runFile = P.interpret $ \case
  ReadFile path -> P.embed $ BS.readFile path
  ReadFileLazy path -> P.embed $ BSL.readFile path
  GetPath path -> P.embed $ getDataFileName path
  ListDirectory path -> P.embed $ listDir @IO path
  ParseAbsDirectory path -> do
    eAbsDir <- runError $ parseAbsDir path
    case eAbsDir of
      Left err -> pure $ Left err
      Right r -> pure $ Right r
