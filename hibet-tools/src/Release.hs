-- | Build the dataset release: @manifest.toml@ plus a gzipped tarball of the
-- data files listed by the manifest.
module Release
    ( ReleaseInfo (..)
    , buildRelease
    ) where

import Manifest
    ( Manifest (..)
    , ManifestFile (..)
    , checkManifest
    , readManifest
    )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tare
import qualified Codec.Compression.GZip as GZip
import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))

data ReleaseInfo = ReleaseInfo
    { releaseVersion :: !Text
    , releaseArchive :: !FilePath
    , releaseManifest :: !FilePath
    , releaseFiles :: !Int
    }
    deriving stock (Eq, Show)

-- | Verify @dataRoot@ against its manifest and write @manifest.toml@ and
-- @hibet-data-<version>.tar.gz@ into @outDir@. Nothing is written when the
-- data does not match the manifest.
buildRelease :: FilePath -> FilePath -> IO (Either String ReleaseInfo)
buildRelease dataRoot outDir = do
    problems <- checkManifest dataRoot
    case problems of
        _ : _ -> pure (Left (T.unpack (T.intercalate "; " problems)))
        [] ->
            readManifest dataRoot >>= \case
                Left err -> pure (Left err)
                Right manifest -> do
                    -- The manifest comes first; its file list keeps the release
                    -- order reproducible.
                    let archiveNames =
                            "manifest.toml" : [file.name | file <- manifest.files]
                    built <- try @IOException $ do
                        createDirectoryIfMissing True outDir
                        let archive =
                                outDir
                                    </> ( "hibet-data-"
                                            <> T.unpack manifest.version
                                            <> ".tar.gz"
                                        )
                            manifestPath = outDir </> "manifest.toml"
                        entries <- mapM (tarEntry dataRoot) archiveNames
                        BSL.writeFile archive (GZip.compress (Tar.write entries))
                        copyFile (dataRoot </> "manifest.toml") manifestPath
                        pure
                            ReleaseInfo
                                { releaseVersion = manifest.version
                                , releaseArchive = archive
                                , releaseManifest = manifestPath
                                , releaseFiles = length manifest.files
                                }
                    pure (either (Left . show) Right built)

tarEntry :: FilePath -> FilePath -> IO Tare.Entry
tarEntry root name = do
    content <- BS.readFile (root </> name)
    case Tare.toTarPath False name of
        Left err -> ioError (userError err)
        Right path -> pure (Tare.fileEntry path (BSL.fromStrict content))
