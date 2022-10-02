module Effects.File where

import Type (HibetError (..))
import Utility (showT)

import Control.Exception (SomeException, fromException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Path (Abs, Dir, File, Path, PathException)
import qualified Path
import Path.IO (listDir)
import Paths_hibet (getDataFileName)
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy as P
import Polysemy.Error (Error, note, throw)


data FileIO m a where
  ReadFile :: FilePath -> FileIO m BS.ByteString
  ReadFileLazy :: FilePath -> FileIO m BSL.ByteString
  GetPath :: FilePath -> FileIO m FilePath
  ListDirectory :: Path b Dir -> FileIO m ([Path Abs Dir], [Path Abs File])
  ParseAbsDir  :: FilePath -> FileIO r (Path Abs Dir)

P.makeSem ''FileIO

runFile :: Members [Embed IO, Error HibetError] r =>
  Sem (FileIO : r) a -> Sem r a
runFile = P.interpret $ \case
  ReadFile path -> P.embed $ BS.readFile path
  ReadFileLazy path -> P.embed $ BSL.readFile path
  GetPath path -> P.embed $ getDataFileName path
  ListDirectory path -> P.embed $ listDir @IO path
  ParseAbsDir path -> parseAbsDirS path


-- Helpers

irrefutablePathException :: (Member (Error HibetError) r)
  => Either SomeException a
  -> Sem r a
irrefutablePathException x = case x of
  Left e -> do
    err <- note (UnknownError $ showT e) $ fromException @PathException e
    throw $ PathError err
  Right a -> pure a

parseAbsDirS ::
  Member (Error HibetError) r =>
  FilePath ->
  Sem r (Path Abs Dir)
parseAbsDirS x = irrefutablePathException $ Path.parseAbsDir x

parseAbsFileS ::
  Member (Error HibetError) r =>
  FilePath ->
  Sem r (Path Abs File)
parseAbsFileS x = irrefutablePathException $ Path.parseAbsFile x
