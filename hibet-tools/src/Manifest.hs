-- | Generate, read and verify @manifest.toml@: the dataset version plus
-- sha256 and size of every file the application reads from the data root.
-- Both @hibet-dataset@ and @hibet-cli@ use this module; the engine does not.
module Manifest
    ( Manifest (..)
    , ManifestError (..)
    , ManifestFile (..)
    , buildManifest
    , checkManifest
    , defaultManifestVersion
    , field
    , intField
    , parseManifest
    , readManifest
    , renderManifestError
    , verifyDir
    , writeManifest
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getZonedTime, localDay, zonedTimeToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (isAbsolute, splitDirectories, (</>))

data Manifest = Manifest
    { schema :: !Int
    , version :: !Text
    , files :: ![ManifestFile]
    }
    deriving stock (Eq, Show)

data ManifestFile = ManifestFile
    { name :: !FilePath
    , sha256 :: !Text
    , size :: !Integer
    }
    deriving stock (Eq, Show)

-- | A problem with @manifest.toml@: the source line when there is one, the
-- manifest key and the reason. @line@ is empty when the problem is not about
-- a value; @fieldName@ is empty when it is about the file as a whole.
data ManifestError = ManifestError
    { line :: !Text
    , fieldName :: !Text
    , reason :: !Text
    }
    deriving stock (Eq, Show)

-- | One readable line for the CLI: the line when it is known, otherwise the
-- key, then the reason.
renderManifestError :: ManifestError -> Text
renderManifestError err
    | not (T.null err.line) = err.line <> ": " <> err.reason
    | not (T.null err.fieldName) = err.fieldName <> ": " <> err.reason
    | otherwise = err.reason

-- | The dataset version is its release date in @YYYY.MM.DD@ form.
defaultManifestVersion :: IO Text
defaultManifestVersion =
    T.pack
        . formatTime defaultTimeLocale "%Y.%m.%d"
        . localDay
        . zonedTimeToLocalTime
        <$> getZonedTime

-- | Every file the application reads from the data root, in manifest order:
-- dictionaries sorted by name, then @titles.toml@ and @key-aliases.tsv@.
manifestFiles :: FilePath -> IO [FilePath]
manifestFiles root = do
    entries <- listDirectory (root </> "dicts")
    pure $
        [ "dicts" </> entry
        | entry <- sortOn (map toLower) entries
        , ".txt" `isSuffixOf` entry
        ]
            <> ["titles.toml", "key-aliases.tsv"]

buildManifest :: FilePath -> Text -> IO Manifest
buildManifest root version = do
    names <- manifestFiles root
    files <- forM names (hashFile root)
    pure Manifest{schema = 1, version = version, files = files}

writeManifest :: FilePath -> Text -> IO (Either ManifestError Int)
writeManifest root version = do
    built <- try @IOException (buildManifest root version)
    case built of
        Left err ->
            pure
                $ Left
                $ ManifestError
                    (T.pack root)
                    ""
                    ("cannot build manifest: " <> T.pack (show err))
        Right manifest -> do
            let path = root </> "manifest.toml"
            written <-
                try @IOException $
                    BS.writeFile path (TE.encodeUtf8 (renderManifest manifest))
            pure $ case written of
                Left err ->
                    Left $
                        ManifestError
                            (T.pack path)
                            ""
                            ("cannot write: " <> T.pack (show err))
                Right () -> Right (length manifest.files)

-- | Read @root/manifest.toml@. Only schema 1 is accepted.
readManifest :: FilePath -> IO (Either ManifestError Manifest)
readManifest root = do
    let path = root </> "manifest.toml"
    exists <- doesFileExist path
    if not exists
        then pure (Left (ManifestError (T.pack path) "" "missing"))
        else do
            loaded <- try @IOException (BS.readFile path)
            pure $ case loaded of
                Left err ->
                    Left $
                        ManifestError
                            (T.pack path)
                            ""
                            ("cannot read: " <> T.pack (show err))
                Right raw -> case TE.decodeUtf8' raw of
                    Left err ->
                        Left $
                            ManifestError
                                (T.pack path)
                                ""
                                ("cannot decode: " <> T.pack (show err))
                    Right content -> parseManifest content

-- | Problems with the manifest itself and with every data file it describes.
-- An empty list means the manifest is current.
checkManifest :: FilePath -> IO [Text]
checkManifest root = do
    manifest <- readManifest root
    case manifest of
        Left err -> pure [renderManifestError err]
        Right current -> do
            checked <- try @IOException (compareFiles root current)
            pure $ case checked of
                Left err ->
                    [ "cannot check "
                        <> T.pack root
                        <> ": "
                        <> T.pack (show err)
                    ]
                Right problems -> problems

compareFiles :: FilePath -> Manifest -> IO [Text]
compareFiles root manifest = do
    actual <- manifestFiles root
    mismatched <- fmap catMaybes . forM actual $ \name ->
        case lookup name [(f.name, f) | f <- manifest.files] of
            Nothing -> pure (Just ("not in manifest: " <> T.pack name))
            Just expected -> do
                current <- try @IOException (hashFile root name)
                pure $ case current of
                    Left err ->
                        Just
                            ( "cannot read "
                                <> T.pack name
                                <> ": "
                                <> T.pack (show err)
                            )
                    Right found
                        | found.size /= expected.size ->
                            Just
                                ( "size mismatch: "
                                    <> T.pack name
                                    <> " (manifest "
                                    <> T.pack (show expected.size)
                                    <> ", file "
                                    <> T.pack (show found.size)
                                    <> ")"
                                )
                        | found.sha256 /= expected.sha256 ->
                            Just ("sha256 mismatch: " <> T.pack name)
                        | otherwise -> Nothing
    let listed = [f.name | f <- manifest.files]
        missing =
            [ "missing: " <> T.pack name
            | name <- listed
            , name `notElem` actual
            ]
    pure (missing <> mismatched)

-- | Problems with the data directory: a file is missing, has a different size
-- or hash, or is not listed in the manifest. @manifest.toml@ itself is ignored.
verifyDir :: FilePath -> Manifest -> IO [Text]
verifyDir root manifest = do
    found <- try @IOException (manifestFiles root)
    case found of
        Left err ->
            pure
                [ "cannot list "
                    <> T.pack (root </> "dicts")
                    <> ": "
                    <> T.pack (show err)
                ]
        Right actual -> do
            listed <- fmap catMaybes . mapM checkFile $ manifest.files
            let expected = [file.name | file <- manifest.files]
                missing =
                    [ "missing: " <> T.pack name
                    | name <- expected
                    , name `notElem` actual
                    ]
                extra =
                    [ "not in manifest: " <> T.pack name
                    | name <- actual
                    , name `notElem` expected
                    ]
            pure (missing <> extra <> listed)
    where
        checkFile file = do
            let path = root </> file.name
            exists <- doesFileExist path
            if not exists
                then pure Nothing
                else do
                    current <- try @IOException (hashFile root file.name)
                    pure $ case current of
                        Left err ->
                            Just
                                ( "cannot read "
                                    <> T.pack file.name
                                    <> ": "
                                    <> T.pack (show err)
                                )
                        Right found
                            | found.size /= file.size ->
                                Just
                                    ( "size mismatch: "
                                        <> T.pack file.name
                                        <> " (manifest "
                                        <> T.pack (show file.size)
                                        <> ", file "
                                        <> T.pack (show found.size)
                                        <> ")"
                                    )
                            | found.sha256 /= file.sha256 ->
                                Just ("sha256 mismatch: " <> T.pack file.name)
                            | otherwise -> Nothing

hashFile :: FilePath -> FilePath -> IO ManifestFile
hashFile root name = do
    bytes <- BS.readFile (root </> name)
    pure
        ManifestFile
            { name = name
            , sha256 = hexSha256 bytes
            , size = fromIntegral (BS.length bytes)
            }

-- | The 'Show' instance of 'Digest' is the lowercase hexadecimal digest.
hexSha256 :: BS.ByteString -> Text
hexSha256 bytes = T.pack (show (hash bytes :: Digest SHA256))

renderManifest :: Manifest -> Text
renderManifest manifest =
    T.unlines $
        [ "# Generated from hibet-data/ on "
            <> T.replace "." "-" manifest.version
            <> ". Do not edit by hand."
        , "schema = " <> T.pack (show manifest.schema)
        , "version = \"" <> manifest.version <> "\""
        ]
            <> concatMap renderFile manifest.files
    where
        renderFile file =
            [ ""
            , "[[files]]"
            , "name = \"" <> T.pack file.name <> "\""
            , "sha256 = \"" <> file.sha256 <> "\""
            , "size = " <> T.pack (show file.size)
            ]

parseManifest :: Text -> Either ManifestError Manifest
parseManifest content = do
    let (header, rest) = break (== "[[files]]") (map T.strip (T.lines content))
    schema <- intField "schema" header
    version <- quotedField "version" header
    files <- traverse parseFileBlock (splitBlocks rest)
    case schema of
        1 -> pure ()
        _ ->
            Left $
                ManifestError
                    ""
                    "schema"
                    ("unsupported schema " <> T.pack (show schema))
    mapM_ checkName files
    pure Manifest{schema = schema, version = version, files = files}
    where
        checkName file
            | safeName file.name = Right ()
            | otherwise =
                Left $
                    ManifestError
                        ""
                        "name"
                        ("unsafe path: " <> T.pack file.name)

parseFileBlock :: [Text] -> Either ManifestError ManifestFile
parseFileBlock block =
    ManifestFile
        <$> (T.unpack <$> quotedField "name" block)
        <*> quotedField "sha256" block
        <*> intField "size" block

splitBlocks :: [Text] -> [[Text]]
splitBlocks [] = []
splitBlocks (line : rest)
    | line == "[[files]]" =
        let (block, rest') = break (== "[[files]]") rest
         in block : splitBlocks rest'
    | otherwise = splitBlocks rest

field :: Text -> [Text] -> Either ManifestError Text
field key content = snd <$> fieldWithLine key content

-- | The parsed value and the source line it came from.
fieldWithLine :: Text -> [Text] -> Either ManifestError (Text, Text)
fieldWithLine key content =
    case [ (line, T.strip (T.drop 1 value))
         | line <- content
         , let (found, value) = T.breakOn "=" line
         , T.strip found == key
         , not (T.null value)
         ] of
        [(line, value)] -> Right (line, value)
        [] -> Left (ManifestError "" key "missing")
        _ -> Left (ManifestError "" key "duplicate")

quotedField :: Text -> [Text] -> Either ManifestError Text
quotedField key content = do
    (line, value) <- fieldWithLine key content
    if T.length value >= 2 && T.head value == '"' && T.last value == '"'
        then Right (T.init (T.tail value))
        else Left (ManifestError line key "not quoted")

intField :: (Read a) => Text -> [Text] -> Either ManifestError a
intField key content = do
    (line, value) <- fieldWithLine key content
    case reads (T.unpack value) of
        [(number, "")] -> Right number
        _ -> Left (ManifestError line key ("not an integer: " <> value))

safeName :: FilePath -> Bool
safeName path =
    not (null path)
        && not (isAbsolute path)
        && all (`notElem` [".", ".."]) (splitDirectories path)
