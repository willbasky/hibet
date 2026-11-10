-- | Install a verified dataset into @$XDG_DATA_HOME/hibet/data@. The old set
-- is kept in @data.old@.
module Install
    ( Installed (..)
    , defaultReleaseSource
    , hibetDataDir
    , installFromDir
    , installFromRelease
    , installWith
    ) where

import Manifest
    ( Manifest (..)
    , ManifestFile (..)
    , parseManifest
    , readManifest
    , verifyDir
    )
import Progress (Progress, beginStep, endStep, updateBytes, withProgress)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception
    ( IOException
    , evaluate
    , onException
    , try
    , uninterruptibleMask_
    )
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
    ( HttpException
    , Manager
    , brRead
    , checkResponse
    , parseRequest
    , responseBody
    , responseHeaders
    , responseStatus
    , withResponse
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hContentLength)
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
installFromDir source targetBase = withProgress $ \progress -> do
    beginStep progress "manifest"
    loaded <- readManifest source
    endStep progress
    case loaded of
        Left err -> pure (Left err)
        Right manifest -> do
            beginStep progress "verify"
            problems <- verifyDir source manifest
            endStep progress
            if null problems
                then installWith progress targetBase manifest $ \stage -> do
                    beginStep progress "copy"
                    copied <- try @IOException $ do
                        forM_ manifest.files $ \file ->
                            copyRelative source stage file.name
                        copyFile
                            (source </> "manifest.toml")
                            (stage </> "manifest.toml")
                    endStep progress
                    pure $
                        either
                            (Left . ("cannot copy data: " <>) . show)
                            Right
                            copied
                else pure (Left (renderProblems problems))

-- | Download @manifest.toml@ and @hibet-data-<version>.tar.gz@ from a release
-- base (HTTP URL, @file://@ URL or local directory) and install them. Returns
-- 'Nothing' when the active set already has the released version.
installFromRelease :: String -> FilePath -> IO (Either String (Maybe Installed))
installFromRelease source targetBase = withProgress $ \progress -> do
    fetch <- makeFetcher source
    beginStep progress "manifest"
    fetchedManifest <- fetch "manifest.toml" (\_ _ -> pure ())
    endStep progress
    case fetchedManifest of
        Left err -> pure (Left err)
        Right raw -> case TE.decodeUtf8' raw of
            Left err ->
                pure (Left ("cannot decode manifest.toml: " <> show err))
            Right content -> case parseManifest content of
                Left err -> pure (Left ("cannot parse manifest.toml: " <> err))
                Right manifest -> do
                    current <- readManifest (targetBase </> "data")
                    case current of
                        Right installed
                            | installed.version == manifest.version ->
                                pure (Right Nothing)
                        _ -> do
                            let archive =
                                    "hibet-data-"
                                        <> T.unpack manifest.version
                                        <> ".tar.gz"
                            beginStep progress "download"
                            fetchedArchive <- fetch archive (updateBytes progress)
                            endStep progress
                            case fetchedArchive of
                                Left err -> pure (Left err)
                                Right bytes -> do
                                    installed <-
                                        installWith progress targetBase manifest $ \stage -> do
                                            beginStep progress "unpack"
                                            extracted <- extractArchive stage manifest bytes
                                            endStep progress
                                            case extracted of
                                                Left err -> pure (Left err)
                                                Right () -> do
                                                    beginStep progress "verify"
                                                    problems <- verifyDir stage manifest
                                                    endStep progress
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
                                    pure (fmap Just installed)

installWith ::
    Progress
    -> FilePath
    -> Manifest
    -> (FilePath -> IO (Either String ()))
    -> IO (Either String Installed)
installWith progress targetBase manifest action = do
    createDirectoryIfMissing True targetBase
    let stage = targetBase </> "data.new"
        cleanup = void (try @IOException (removePathForcibly stage))
        install = do
            prepared <- action stage
            case prepared of
                Left err -> cleanup >> pure (Left err)
                Right () -> do
                    beginStep progress "switch"
                    switched <- swapIn targetBase stage
                    endStep progress
                    case switched of
                        Left err -> pure (Left err)
                        Right path -> do
                            beginStep progress "done"
                            endStep progress
                            pure
                                ( Right
                                    Installed
                                        { installedVersion = manifest.version
                                        , installedFiles = length manifest.files
                                        , installedPath = path
                                        }
                                )
    removePathForcibly stage
    install `onException` cleanup

copyRelative :: FilePath -> FilePath -> FilePath -> IO ()
copyRelative source stage name = do
    let dest = stage </> name
    createDirectoryIfMissing True (takeDirectory dest)
    copyFile (source </> name) dest

-- | Remove @data.old@, move @data@ aside and move the staged set in place.
-- Masked from asynchronous exceptions: a Ctrl+C must not leave no active set.
swapIn :: FilePath -> FilePath -> IO (Either String FilePath)
swapIn targetBase stage = do
    let active = targetBase </> "data"
        old = targetBase </> "data.old"
    result <- try @IOException $ uninterruptibleMask_ $ do
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

type Fetch =
    FilePath
    -> (Int64 -> Maybe Int64 -> IO ())
    -> IO (Either String BS.ByteString)

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
localFetcher dir name report = do
    result <- try @IOException (BS.readFile (dir </> name))
    case result of
        Left err -> pure (Left (show err))
        Right bytes -> do
            report (fromIntegral (BS.length bytes)) Nothing
            pure (Right bytes)

httpFetcher :: Manager -> String -> Fetch
httpFetcher manager base name report = do
    let url = joinUrl base name
    result <- try @HttpException $ do
        request0 <- parseRequest url
        let request = request0{checkResponse = \_ _ -> pure ()}
        withResponse request manager $ \response -> do
            let code = statusCode (responseStatus response)
            if code >= 200 && code < 300
                then do
                    let total =
                            lookup hContentLength (responseHeaders response)
                                >>= parseLength
                    collect total (responseBody response) 0 []
                else pure (Left ("HTTP " <> show code <> " for " <> url))
    pure (either (Left . show) id result)
    where
        collect total body done chunks = do
            chunk <- brRead body
            if BS.null chunk
                then pure (Right (BS.concat (reverse chunks)))
                else do
                    let done' = done + fromIntegral (BS.length chunk)
                    done' `seq` report done' total
                    collect total body done' (chunk : chunks)

parseLength :: BS.ByteString -> Maybe Int64
parseLength raw = case reads (BSC.unpack raw) of
    [(size, "")] -> Just size
    _ -> Nothing

joinUrl :: String -> FilePath -> String
joinUrl base name = dropWhileEnd (== '/') base <> "/" <> name
