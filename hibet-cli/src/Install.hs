-- | Install a verified dataset into @$XDG_DATA_HOME/hibet/data@. The old set
-- is kept in @data.old@.
module Install
    ( Installed (..)
    , defaultReleaseSource
    , hibetDataDir
    , installFromDir
    , installFromRelease
    ) where

import Manifest
    ( Manifest (..)
    , ManifestFile (..)
    , parseManifest
    , readManifest
    , verifyDir
    )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (dropWhileEnd, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
    ( HttpException
    , Manager
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
    ( XdgDirectory (XdgData)
    , copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , getXdgDirectory
    , removePathForcibly
    , renameDirectory
    )
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))

data Installed = Installed
    { installedVersion :: !Text
    , installedFiles :: !Int
    , installedPath :: !FilePath
    }
    deriving stock (Eq, Show)

defaultReleaseSource :: String
defaultReleaseSource =
    "https://github.com/willbasky/hibet/releases/download/hibet-data"

hibetDataDir :: IO FilePath
hibetDataDir = getXdgDirectory XdgData "hibet"

-- | Copy a verified data directory (with @manifest.toml@, @titles.toml@,
-- @key-aliases.tsv@ and @dicts/@) into the XDG data directory.
installFromDir :: FilePath -> FilePath -> IO (Either String Installed)
installFromDir source targetBase =
    readManifest source >>= \case
        Left err -> pure (Left err)
        Right manifest -> do
            problems <- verifyDir source manifest
            if null problems
                then installWith targetBase manifest $ \stage -> do
                    copied <- try @IOException $ do
                        forM_ manifest.files $ \file ->
                            copyRelative source stage file.name
                        copyFile
                            (source </> "manifest.toml")
                            (stage </> "manifest.toml")
                    pure $
                        either
                            (Left . ("cannot copy data: " <>) . show)
                            Right
                            copied
                else pure (Left (renderProblems problems))

-- | Download @manifest.toml@ and @hibet-data-<version>.tar.gz@ from a release
-- base (HTTP URL, @file://@ URL or local directory) and install them.
installFromRelease :: String -> FilePath -> IO (Either String Installed)
installFromRelease source targetBase = do
    fetch <- makeFetcher source
    fetchedManifest <- fetch "manifest.toml"
    case fetchedManifest of
        Left err -> pure (Left err)
        Right raw -> case TE.decodeUtf8' raw of
            Left err ->
                pure (Left ("cannot decode manifest.toml: " <> show err))
            Right content -> case parseManifest content of
                Left err -> pure (Left ("cannot parse manifest.toml: " <> err))
                Right manifest -> do
                    let archive =
                            "hibet-data-" <> T.unpack manifest.version <> ".tar.gz"
                    fetchedArchive <- fetch archive
                    case fetchedArchive of
                        Left err -> pure (Left err)
                        Right bytes ->
                            installWith targetBase manifest $ \stage -> do
                                extracted <- extractArchive stage manifest bytes
                                case extracted of
                                    Left err -> pure (Left err)
                                    Right () -> do
                                        problems <- verifyDir stage manifest
                                        if null problems
                                            then do
                                                written <-
                                                    try @IOException $
                                                        BS.writeFile
                                                            (stage </> "manifest.toml")
                                                            raw
                                                pure $
                                                    either
                                                        (Left . show)
                                                        Right
                                                        written
                                            else pure (Left (renderProblems problems))

installWith ::
    FilePath
    -> Manifest
    -> (FilePath -> IO (Either String ()))
    -> IO (Either String Installed)
installWith targetBase manifest action = do
    createDirectoryIfMissing True targetBase
    let stage = targetBase </> "data.new"
    removePathForcibly stage
    prepared <- action stage
    case prepared of
        Left err -> removePathForcibly stage >> pure (Left err)
        Right () -> do
            switched <- swapIn targetBase stage
            pure $ case switched of
                Left err -> Left err
                Right path ->
                    Right
                        Installed
                            { installedVersion = manifest.version
                            , installedFiles = length manifest.files
                            , installedPath = path
                            }

copyRelative :: FilePath -> FilePath -> FilePath -> IO ()
copyRelative source stage name = do
    let dest = stage </> name
    createDirectoryIfMissing True (takeDirectory dest)
    copyFile (source </> name) dest

swapIn :: FilePath -> FilePath -> IO (Either String FilePath)
swapIn targetBase stage = do
    let active = targetBase </> "data"
        old = targetBase </> "data.old"
    result <- try @IOException $ do
        activeExists <- doesDirectoryExist active
        when activeExists $ do
            removePathForcibly old
            renameDirectory active old
        renameDirectory stage active
    pure $ case result of
        Left err -> Left ("cannot switch data: " <> show err)
        Right () -> Right active

renderProblems :: [Text] -> String
renderProblems = T.unpack . T.intercalate "; "

-- | Extract only the files named by the manifest plus @manifest.toml@.
-- Directories are created on demand; links and device files are rejected.
extractArchive :: FilePath -> Manifest -> BS.ByteString -> IO (Either String ())
extractArchive stage manifest bytes = do
    forced <-
        try @IOException
            $ try @GZip.DecompressError
            $ evaluate (BSL.toStrict (GZip.decompress (BSL.fromStrict bytes)))
    case forced of
        Left err -> pure (Left ("cannot decompress archive: " <> show err))
        Right (Left err) -> pure (Left ("cannot decompress archive: " <> show err))
        Right (Right unpacked) -> do
            extracted <-
                try @IOException $
                    unpackEntries stage allowed (Tar.read (BSL.fromStrict unpacked))
            pure $ case extracted of
                Left err -> Left ("cannot extract archive: " <> show err)
                Right result -> result
    where
        allowed = "manifest.toml" : [file.name | file <- manifest.files]

unpackEntries ::
    FilePath
    -> [FilePath]
    -> Tar.Entries Tar.FormatError
    -> IO (Either String ())
unpackEntries stage allowed = \case
    Tar.Fail err -> pure (Left ("broken archive: " <> show err))
    Tar.Done -> pure (Right ())
    Tar.Next entry rest -> do
        let path = Tar.entryPath entry
        if not (safeArchivePath path)
            then pure (Left ("unsafe archive path: " <> path))
            else case Tar.entryContent entry of
                Tar.NormalFile content _
                    | path `notElem` allowed ->
                        pure (Left ("unexpected archive file: " <> path))
                    | otherwise -> do
                        let dest = stage </> path
                        createDirectoryIfMissing True (takeDirectory dest)
                        BSL.writeFile dest content
                        unpackEntries stage allowed rest
                Tar.Directory -> unpackEntries stage allowed rest
                _ -> pure (Left ("unsupported archive entry: " <> path))

safeArchivePath :: FilePath -> Bool
safeArchivePath path =
    not (null path)
        && not (isAbsolute path)
        && all (`notElem` [".", ".."]) (splitDirectories path)

type Fetch = FilePath -> IO (Either String BS.ByteString)

makeFetcher :: String -> IO Fetch
makeFetcher source
    | Just dir <- stripPrefix "file://" source = pure (localFetcher dir)
    | otherwise = do
        isDir <- doesDirectoryExist source
        if isDir
            then pure (localFetcher source)
            else do
                manager <- newTlsManager
                pure (httpFetcher manager source)

localFetcher :: FilePath -> Fetch
localFetcher dir name = do
    result <- try @IOException (BS.readFile (dir </> name))
    pure (either (Left . show) Right result)

httpFetcher :: Manager -> String -> Fetch
httpFetcher manager base name = do
    let url = joinUrl base name
    result <- try @HttpException $ do
        request <- parseRequest url
        response <- httpLbs request manager
        let code = statusCode (responseStatus response)
        pure $
            if code >= 200 && code < 300
                then Right (BSL.toStrict (responseBody response))
                else Left ("HTTP " <> show code <> " for " <> url)
    pure (either (Left . show) id result)

joinUrl :: String -> FilePath -> String
joinUrl base name = dropWhileEnd (== '/') base <> "/" <> name
