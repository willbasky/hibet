module Effects.File where

import Type (HibetError (..))
import Effects.Common (adapt)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Path (Abs, Dir, File, Path)
import qualified Path
import Path.IO (listDir)
import Paths_hibet (getDataFileName)

import Effectful.TH ( makeEffect )
import Effectful
    ( type (:>),
      Effect,
      Dispatch(Dynamic),
      DispatchOf,
      Eff,
      IOE )
import Effectful.Error.Static  ( Error )
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
  )
  => Eff (FileSystem : es) a
  -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
    ReadFileBS path -> adapt $ BS.readFile path
    ReadFileLazyBS path  -> adapt $ BSL.readFile path
    GetPath path -> adapt $ getDataFileName path
    ListDirectory path -> adapt $ listDir @IO path
    ParseAbsDir path -> adapt $ Path.parseAbsDir path
