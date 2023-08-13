module Effects.File where

import Type (HibetError (..))
import Utility (showT)

import Control.Exception (SomeException, IOException, fromException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Path (Abs, Dir, File, Path, PathException)
import qualified Path
import Path.IO (listDir)
import Paths_hibet (getDataFileName)

import Effectful.TH ( makeEffect )
import Effectful
    ( MonadIO(liftIO),
      type (:>),
      Effect,
      Dispatch(Dynamic),
      DispatchOf,
      Eff,
      IOE )
import Effectful.Error.Static
    ( CallStack, prettyCallStack, Error, catchError, throwError )
import Effectful.Dispatch.Dynamic ( interpret )

data FileSystem :: Effect where
  ReadFileBS :: FilePath -> FileSystem m BS.ByteString
  ReadFileLazyBS :: FilePath -> FileSystem m BSL.ByteString
  GetPath :: FilePath -> FileSystem m FilePath
  ListDirectory :: Path b Dir -> FileSystem m ([Path Abs Dir], [Path Abs File])
  ParseAbsDir :: FilePath -> FileSystem r (Path Abs Dir)

type instance DispatchOf FileSystem = 'Dynamic

makeEffect ''FileSystem

runFileSystemIO ::
  ( IOE :> es
  , Error HibetError :> es
  , Error SomeException :> es
  )
  => Eff (FileSystem : es) a
  -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
    ReadFileBS path -> adapt $ BS.readFile path
    ReadFileLazyBS path  -> adapt $ BSL.readFile path
    GetPath path -> adapt $ getDataFileName path
    ListDirectory path -> adapt $ listDir @IO path
    ParseAbsDir path -> adapt $ Path.parseAbsDir path

adapt ::
  ( IOE :> es
  , Error HibetError :> es
  , Error SomeException :> es
  )
  => IO a -> Eff es a
adapt m = catchError (liftIO m) $
  \(stack :: CallStack) (e :: SomeException) ->
    case fromException @IOException e of
      Just ioErr -> throwError $ FileError ioErr (showT $ prettyCallStack stack)
      Nothing -> case fromException @PathException e of
        Just pathErr -> throwError $ PathError pathErr
        Nothing -> throwError $ UnknownError $ showT e



