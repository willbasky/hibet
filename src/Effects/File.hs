module Effects.File where

import Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Paths_hibet (getDataFileName)
import Polysemy (Embed, Members, Sem)
import Polysemy.Error (Error, mapError)
import qualified Polysemy as P
import Polysemy.Path (Abs, File, Path, Dir, parseAbsDir)
import Path.IO (listDir)


data FileIO m a where
  ReadFile :: FilePath -> FileIO m BS.ByteString
  ReadFileLazy :: FilePath -> FileIO m BSL.ByteString
  GetPath :: FilePath -> FileIO m FilePath
  ListDirectory :: Path b Dir -> FileIO m ([Path Abs Dir], [Path Abs File])
  ParseAbsDirectory :: FilePath -> FileIO r (Path Abs Dir)

P.makeSem ''FileIO

runFile :: Members [Embed IO, Error HibetErrors] r => Sem (FileIO : r) a -> Sem r a
runFile = P.interpret $ \case
  ReadFile path -> P.embed $ BS.readFile path
  ReadFileLazy path -> P.embed $ BSL.readFile path
  GetPath path -> P.embed $ getDataFileName path
  ListDirectory path -> P.embed $ listDir @IO path
  ParseAbsDirectory path -> mapError PathError $ parseAbsDir path





