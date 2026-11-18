module Type
    ( -- * Error
      HibetError (..)
    , renderError
    ) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T

-- | Why a command could not do its work. The constructors name the cause, not
-- the mechanism: no data root, an IO failure, broken data, a rejected query.
data HibetError
    = DataNotFound [FilePath]
    | DataUnreadable FilePath Text
    | DataInvalid FilePath Text
    | QueryInvalid Text
    deriving stock (Eq, Show)

instance Exception HibetError

-- | Human-readable error text: summary and detail, no constructor names.
renderError :: HibetError -> Text
renderError = \case
    DataNotFound paths ->
        "Dictionary data not found\nlooked in: "
            <> T.intercalate ", " (map T.pack paths)
            <> ". Install it with `hibet data install --from DIR`"
            <> " or `hibet data update`, or pass --data-dir=DIR /"
            <> " set HIBET_DATA_DIR to a data root with titles.toml"
            <> " and dicts/."
    DataUnreadable path detail ->
        "Failed to read " <> T.pack path <> "\n" <> detail
    DataInvalid path detail ->
        "Invalid data in " <> T.pack path <> "\n" <> detail
    QueryInvalid detail -> "Invalid query\n" <> detail
