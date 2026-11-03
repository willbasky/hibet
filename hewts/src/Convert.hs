module Convert
    ( splitSentences
    , splitSentencesWith
    , InputFormat (..)
    , Settings (..)
    , defaultSettings
    , convertWylieToUnicode
    , convertUnicodeToWylie
    , checkSpelling
    , syllables
    , renderItems
    , legality
    , documentDiagnostics
    , OutputFormat (..)
    , SpellItem (..)
    , pSentence
    ) where

import Convert.Diagnostic (Diagnostics, addDiagnostic, noTibetanCharacters)
import Convert.Grammar.Parser (parseEither)
import Convert.Grammar.Sentence (SpellItem (..), Syllable (..), pSentence)
import Convert.Render (OutputFormat (..), renderItems)
import Convert.Render.Unicode (renderSyllable)
import Convert.Token (TokenCanonical (TcUnknown), mkSpan, tokenCanonical)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Text (Text)
import qualified Data.Text as T

-- | Split Tibetan text into a structured token stream: every token is
-- preserved, syllables are recognized via the 37 grammar structures,
-- digits become 'Number', punctuation and whitespace become 'Punct',
-- and anything unrecognized becomes 'Other'.
splitSentences :: Text -> Either Text [SpellItem]
splitSentences input =
    case splitSentencesWith input of
        Left err -> Left err
        Right (items, _) -> Right items

-- | Like 'splitSentences', but also hands back what the tokenizer noticed
-- while reading the text and what the grammar's constraint windows found for
-- each syllable run.
--
-- The document-level warning is **not** part of this contract: it needs the
-- input as a whole and is nobody's per-run finding, so the converters
-- ('convertWylieToUnicode', 'convertUnicodeToWylie') add it themselves. A
-- caller that assembles a conversion by hand has to add
-- 'documentDiagnostics' too (that was O6 in `REFACTOR-AST.md` §13.5).
splitSentencesWith :: Text -> Either Text ([SpellItem], Diagnostics)
splitSentencesWith = parseSettings defaultSettings

-- | Which tokenizer reads the input. There is no auto-detection: a Latin
-- letter may be foreign text inside Tibetan (@bi na ya@), so the caller says
-- which writing it hands over - the reference keeps two entry points as well.
data InputFormat
    = InWylie
    | InUnicode
    deriving (Show, Eq)

-- | What the caller may switch off. 'withChecks' is the difference between the
-- two modes of the product: with it off the conversion is a fast pass that
-- records nothing.
--
-- There is no strict/normal split in this record yet: 'Severity' is
-- warning/error, and which of our findings belong to a strict mode is a
-- decision that has not been made (ROADMAP.md, т. 6). A field that no code
-- reads would be worse than none.
data Settings = Settings
    { inputFormat :: InputFormat
    , withChecks :: Bool
    }
    deriving (Show, Eq)

-- | Tibetan script in, checks on.
defaultSettings :: Settings
defaultSettings = Settings{inputFormat = InUnicode, withChecks = True}

-- | The one door both directions use: tokenize the writing the settings name,
-- parse it, and collect what the tokenizer noticed and the grammar found.
parseSettings :: Settings -> Text -> Either Text ([SpellItem], Diagnostics)
parseSettings st input = do
    let (tokens, noticed) = case inputFormat st of
            InWylie -> tokenizeWylie input
            InUnicode -> tokenizeUnicode input
    items <- parseEither pSentence tokens
    pure (items, if withChecks st then noticed <> legality items else mempty)

-- | Wylie to the Tibetan script, with the findings.
convertWylieToUnicode :: Settings -> Text -> Either Text (Text, Diagnostics)
convertWylieToUnicode st = convert st{inputFormat = InWylie} OutUnicode

-- | The Tibetan script to Wylie, with the findings.
convertUnicodeToWylie :: Settings -> Text -> Either Text (Text, Diagnostics)
convertUnicodeToWylie st = convert st{inputFormat = InUnicode} OutWylie

-- | Spelling only: the same findings the converters report, and no text. The
-- input is not touched - the result is the run's records. Checks are this
-- function's whole job, so 'withChecks' is not consulted here.
checkSpelling :: Settings -> Text -> Either Text Diagnostics
checkSpelling st input = do
    (items, diags) <- parseSettings st{withChecks = True} input
    pure (diags <> documentDiagnostics input items)

-- | One direction of the conversion: the text of the target writing and the
-- findings, the document-level warning among them.
convert :: Settings -> OutputFormat -> Text -> Either Text (Text, Diagnostics)
convert st fmt input = do
    (items, diags) <- parseSettings st input
    let document = if withChecks st then documentDiagnostics input items else mempty
    pure (renderItems fmt items, diags <> document)

-- | The spelling warnings for a whole parsed sentence: the 'sylDiags' of
-- every syllable run, in source order.
legality :: [SpellItem] -> Diagnostics
legality = foldMap itemDiags
    where
        itemDiags (SyllableItem s) = sylDiags s
        itemDiags (InvalidSyllableItem _ d) = d
        itemDiags _ = mempty

-- | The document-level warning: the whole input holds no single element the
-- converter reads as Tibetan. A syllable (sound or broken), a number and a
-- sign all count; a bracketed block of foreign text and a stray letter or
-- special mark do not, so a document of nothing but those reads as empty and
-- is blamed whole. The span is the whole input and no word is quoted - the
-- warning is about the document, not about any run in it.
--
-- This is the W->U arm of the reference, whose @units == 0@ control counts a
-- letter, a number or a sign of punctuation; the W->U assembly calls it (see
-- @Test.Parity.convertW2U@). The predicate is about positions the tokenizers
-- already set, not a second walk over the tokens.
documentDiagnostics :: Text -> [SpellItem] -> Diagnostics
documentDiagnostics input items
    | hasTibetanElement items = mempty
    | otherwise =
        addDiagnostic
            (noTibetanCharacters (mkSpan 0 (fromIntegral (T.length input))))
            mempty

-- | Whether the parsed sentence holds at least one element the converter reads
-- as Tibetan: a syllable (sound or broken), a number, or any token that took a
-- canonical payload (a sign, a punctuation mark, a space, a symbol). A token
-- that stayed 'TcUnknown' - an ASCII letter, an unmatched special mark, the
-- content of a bracket block - is no such element.
hasTibetanElement :: [SpellItem] -> Bool
hasTibetanElement = any itemHoldsElement
    where
        itemHoldsElement (SyllableItem _) = True
        itemHoldsElement (InvalidSyllableItem _ _) = True
        itemHoldsElement (Number _) = True
        itemHoldsElement (Punct ts) = any isElement ts
        itemHoldsElement (Other ts) = any isElement ts
        isElement t = case tokenCanonical t of
            TcUnknown _ -> False
            _ -> True

-- | Extract only the recognized syllables from Tibetan text, each in the
-- canonical spelling (REFACTOR-AST Q8: the same fields, in the same order, as
-- 'renderItems' 'OutUnicode' prints - so @syllables "བཾྲ"@ is @"བྲཾ"@).
syllables :: Text -> Either Text [Text]
syllables input = do
    items <- splitSentences input
    pure [renderSyllable s | SyllableItem s <- items]
