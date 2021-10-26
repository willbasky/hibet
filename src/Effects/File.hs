module Effects.File where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Path.IO (listDir)
import Paths_hibet (getDataFileName)
import Polysemy (Embed, Members, Member, Sem)
import qualified Polysemy as P
import Polysemy.Error (Error, mapError)
import Polysemy.Path (Abs, Dir, File, Path, PathException)
import qualified Polysemy.Path as PP

data HibetError
  = PathError PathException
  | UnknownError Text
  deriving stock (Eq, Show)

data FileIO m a where
  ReadFile :: FilePath -> FileIO m BS.ByteString
  ReadFileLazy :: FilePath -> FileIO m BSL.ByteString
  GetPath :: FilePath -> FileIO m FilePath
  ListDirectory :: Path b Dir -> FileIO m ([Path Abs Dir], [Path Abs File])
  ParseAbsDir  :: FilePath -> FileIO r (Path Abs Dir)

P.makeSem ''FileIO

runFile :: Members [Embed IO, Error HibetError] r => Sem (FileIO : r) a -> Sem r a
runFile = P.interpret $ \case
  ReadFile path -> P.embed $ BS.readFile path
  ReadFileLazy path -> P.embed $ BSL.readFile path
  GetPath path -> P.embed $ getDataFileName path
  ListDirectory path -> P.embed $ listDir @IO path
  ParseAbsDir path -> mapErr $ PP.parseAbsDir path

mapErr :: Member (Error HibetError) r
  => Sem (Error PathException : r) a -> Sem r a
mapErr = mapError PathError
