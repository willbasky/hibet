-- | Read and verify @manifest.toml@ before installing a dataset. The library
-- @hibet@ deliberately does not carry a manifest parser.
module Manifest
    ( Manifest (..)
    , ManifestFile (..)
    , parseManifest
    , readManifest
    , verifyDir
    ) where

import Control.Exception (IOException, try)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

-- | Read @root/manifest.toml@. Only schema 1 is accepted.
readManifest :: FilePath -> IO (Either String Manifest)
readManifest root = do
    let path = root </> "manifest.toml"
    exists <- doesFileExist path
    if not exists
        then pure (Left ("missing " <> path))
        else do
            loaded <- try @IOException (BS.readFile path)
            pure $ case loaded of
                Left err -> Left ("cannot read " <> path <> ": " <> show err)
                Right raw -> case TE.decodeUtf8' raw of
                    Left err -> Left ("cannot decode " <> path <> ": " <> show err)
                    Right content -> parseManifest content

parseManifest :: Text -> Either String Manifest
parseManifest content = do
    let (header, rest) = break (== "[[files]]") (map T.strip (T.lines content))
    schema <- intField "schema" header
    version <- quotedField "version" header
    files <- traverse parseFileBlock (splitBlocks rest)
    case schema of
        1 -> pure ()
        _ -> Left ("unsupported schema " <> show schema)
    mapM_ checkName files
    pure Manifest{schema = schema, version = version, files = files}
    where
        checkName file
            | safeName file.name = Right ()
            | otherwise = Left ("unsafe path in manifest: " <> file.name)

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
                    current <- try @IOException (hashFile path)
                    pure $ case current of
                        Left err ->
                            Just
                                ( "cannot read "
                                    <> T.pack file.name
                                    <> ": "
                                    <> T.pack (show err)
                                )
                        Right (digest, len)
                            | len /= file.size ->
                                Just
                                    ( "size mismatch: "
                                        <> T.pack file.name
                                        <> " (manifest "
                                        <> T.pack (show file.size)
                                        <> ", file "
                                        <> T.pack (show len)
                                        <> ")"
                                    )
                            | digest /= file.sha256 ->
                                Just ("sha256 mismatch: " <> T.pack file.name)
                            | otherwise -> Nothing

-- | Files the installer owns, in the order used by @hibet-tools@.
manifestFiles :: FilePath -> IO [FilePath]
manifestFiles root = do
    entries <- listDirectory (root </> "dicts")
    pure $
        [ "dicts" </> entry
        | entry <- sortOn (map toLower) entries
        , ".txt" `isSuffixOf` entry
        ]
            <> ["titles.toml", "key-aliases.tsv"]

hashFile :: FilePath -> IO (Text, Integer)
hashFile path = do
    bytes <- BS.readFile path
    pure
        ( T.pack (show (hash bytes :: Digest SHA256))
        , fromIntegral (BS.length bytes)
        )

safeName :: FilePath -> Bool
safeName path =
    not (null path)
        && not (isAbsolute path)
        && all (`notElem` [".", ".."]) (splitDirectories path)

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
