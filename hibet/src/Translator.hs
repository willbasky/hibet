module Translator
    ( getAnswer
    , tibetToWylieKey
    )
where

import Convert
    ( OutputFormat (OutWylie)
    , SpellItem (SyllableItem)
    , convertWylieToUnicode
    , defaultSettings
    , renderItems
    , splitSentences
    )
import Dictionary (Answer, searchTranslation)
import Env (Env (..))
import Type (HibetError (..))

import Data.Either.Extra (mapLeft)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

getAnswer :: Text -> Env -> Either HibetError (Text, [Answer])
getAnswer query env = do
    -- 1. Detect script of input
    let script = detectScript query
    -- 2. Prepare wylie and tibetan scripts
    (queryWylie, queryTibet) <- boilQuery script query
    -- 3. Translate
    let dscValues = mapMaybe (searchTranslation queryWylie) env.dictionaryMeta
    -- 5. Return
    pure (queryTibet, dscValues)

-- | Which writing the query is in. `hewts` has no auto-detection: a Latin
-- letter may be foreign text inside Tibetan, so the caller says the writing.
-- The Tibetan block is enough here: EWTS input has no characters in it.
data ScriptType = Tibet | Wylie

detectScript :: Text -> ScriptType
detectScript query
    | T.any isTibetan query = Tibet
    | otherwise = Wylie
    where
        isTibetan c = c >= '\x0f00' && c <= '\x0fff'

boilQuery ::
    ScriptType
    -> Text
    -> Either HibetError (Text, Text)
boilQuery script query = case script of
    Wylie -> do
        tibetanText <- fromHewts $ fst <$> convertWylieToUnicode defaultSettings query
        pure (T.unwords (T.words query), tibetanText)
    Tibet -> do
        wylieText <- tibetToWylieKey query
        pure (wylieText, T.strip query)

-- | The Wylie search key for Tibetan script. The dictionary is keyed by Wylie
-- words, so punctuation and spaces of the query must not reach the key: take
-- the syllables through the token layer. The same function names the keys in
-- @data/key-aliases.tsv@, so a Unicode query lands on the canonical spelling
-- of a legacy key.
tibetToWylieKey :: Text -> Either HibetError Text
tibetToWylieKey query = do
    items <- mapLeft UnknownError $ splitSentences query
    pure $
        T.unwords
            [renderItems OutWylie [item] | item@SyllableItem{} <- items]

fromHewts :: Either Text a -> Either HibetError a
fromHewts = mapLeft UnknownError
