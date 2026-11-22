-- | The file boundary: text is 'Text' in the domain, files are strictly
-- read and written as UTF-8 'ByteString'. The locale is not consulted, so a
-- dictionary reads the same under @LANG=C@ and a UTF-8 locale.
module Io
    ( readUtf8
    , writeUtf8
    , Utf8Error (..)
    , renderUtf8Error
    , toHibetError
    ) where

import Type (HibetError (..))

import Control.Exception (Exception, IOException, displayException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)

-- | Why a file could not be used as UTF-8 text. The path is kept so callers
-- can name the file in their own error type.
data Utf8Error
    = Unreadable FilePath IOException
    | InvalidUtf8 FilePath UnicodeException
    deriving stock (Show)

instance Exception Utf8Error

-- | Read a file strictly and decode it as UTF-8.
readUtf8 :: FilePath -> IO (Either Utf8Error Text)
readUtf8 path = do
    bytes <- try @IOException (BS.readFile path)
    pure $ case bytes of
        Left err -> Left (Unreadable path err)
        Right raw -> case TE.decodeUtf8' raw of
            Left err -> Left (InvalidUtf8 path err)
            Right text -> Right text

-- | Write text as UTF-8 bytes, strictly.
writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 path = BS.writeFile path . TE.encodeUtf8

-- | The detail of an error without the path, for callers that already name
-- the file.
renderUtf8Error :: Utf8Error -> Text
renderUtf8Error = \case
    Unreadable _ err -> T.pack (displayException err)
    InvalidUtf8 _ err -> T.pack (show err)

-- | Translate to the engine error, keeping the path and the cause.
toHibetError :: Utf8Error -> HibetError
toHibetError err = case err of
    Unreadable path _ -> DataUnreadable path (renderUtf8Error err)
    InvalidUtf8 path _ -> DataInvalid path (renderUtf8Error err)
