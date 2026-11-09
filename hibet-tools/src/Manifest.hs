-- | Generate and verify @data/manifest.toml@: the dataset version plus sha256
-- and size of every file the application reads from the data root.
module Manifest
    ( Manifest (..)
    , ManifestFile (..)
    , buildManifest
    , checkManifest
    , defaultManifestVersion
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
import System.FilePath ((</>))

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

writeManifest :: FilePath -> Text -> IO (Either String Int)
writeManifest root version = do
    built <- try @IOException (buildManifest root version)
    case built of
        Left err -> pure (Left ("cannot build manifest: " <> show err))
        Right manifest -> do
            let path = root </> "manifest.toml"
            written <-
                try @IOException $
                    BS.writeFile path (TE.encodeUtf8 (renderManifest manifest))
            pure $ case written of
                Left err -> Left ("cannot write " <> path <> ": " <> show err)
                Right () -> Right (length manifest.files)

-- | Problems with the manifest itself and with every data file it describes.
-- An empty list means the manifest is current.
checkManifest :: FilePath -> IO [Text]
checkManifest root = do
    let path = root </> "manifest.toml"
    exists <- doesFileExist path
    if not exists
        then pure ["missing " <> T.pack path]
        else do
            loaded <- try @IOException (BS.readFile path)
            case loaded of
                Left err ->
                    pure ["cannot read " <> T.pack path <> ": " <> T.pack (show err)]
                Right raw -> case TE.decodeUtf8' raw of
                    Left err ->
                        pure
                            [ "cannot decode " <> T.pack path <> ": " <> T.pack (show err)
                            ]
                    Right content -> case parseManifest content of
                        Left err ->
                            pure ["cannot parse " <> T.pack path <> ": " <> T.pack err]
                        Right manifest
                            | manifest.schema /= 1 ->
                                pure
                                    [ "unsupported schema "
                                        <> T.pack (show manifest.schema)
                                    ]
                            | otherwise -> do
                                checked <-
                                    try @IOException (compareFiles root manifest)
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
        [ "# Generated from data/ on "
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

parseManifest :: Text -> Either String Manifest
parseManifest content = do
    let (header, rest) = break (== "[[files]]") (map T.strip (T.lines content))
    schema <- intField "schema" header
    version <- quotedField "version" header
    files <- traverse parseFileBlock (splitBlocks rest)
    pure Manifest{schema = schema, version = version, files = files}

parseFileBlock :: [Text] -> Either String ManifestFile
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

field :: Text -> [Text] -> Either String Text
field key content =
    case [ T.strip (T.drop 1 value)
         | line <- content
         , let (found, value) = T.breakOn "=" line
         , T.strip found == key
         , not (T.null value)
         ] of
        [value] -> Right value
        [] -> Left ("missing " <> T.unpack key)
        _ -> Left ("duplicate " <> T.unpack key)

quotedField :: Text -> [Text] -> Either String Text
quotedField key content = do
    value <- field key content
    if T.length value >= 2 && T.head value == '"' && T.last value == '"'
        then Right (T.init (T.tail value))
        else Left ("unquoted " <> T.unpack key)

intField :: (Read a) => Text -> [Text] -> Either String a
intField key content = do
    value <- field key content
    case reads (T.unpack value) of
        [(number, "")] -> Right number
        _ -> Left ("bad " <> T.unpack key)
