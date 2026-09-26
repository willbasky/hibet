module Convert
    ( splitSentences
    , splitSentencesWith
    , syllables
    , renderItems
    , OutputFormat (..)
    , SpellItem (..)
    , pSentence
    ) where

import Convert.Diagnostic (Diagnostics)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Grammar.Word (Position (..))
import Convert.Sentence (SpellItem (..), pSentence)
import Convert.Token
    ( FinalMark (FMCaret)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , UnknownMark (..)
    , subjoinOf
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode, unicodeOf)
import Convert.Tokenizer.Wylie (wylieOf)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
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
-- while reading the text.
splitSentencesWith :: Text -> Either Text ([SpellItem], Diagnostics)
splitSentencesWith input = do
    let (tokens, diagnostics) = tokenizeUnicode input
    items <- parseEither (pSentence Tibetan) tokens
    pure (items, diagnostics)

-- | Extract only the recognized syllables from Tibetan text, each as its
-- raw spelling (e.g. @མཆོག་དེ ' ->
-- @["མཆོག","དེ"]@).
syllables :: Text -> Either Text [Text]
syllables input = do
    items <- splitSentences input
    pure [T.concat (toList (fmap (tokenRaw . snd) ts)) | Syllable ts <- items]

-- | The script used for 'renderItems' conversion.
data OutputFormat
    = OutUnicode
    | OutWylie
    deriving (Show, Eq)

-- | Convert every token to an 'OutputFormat' spelling, preserving
-- everything: same-script tokens keep their exact raw spelling (including
-- aliases), cross-script tokens become canonical representatives, and
-- tokens without a cross-script spelling fall back to their raw spelling.
renderItems :: OutputFormat -> [SpellItem] -> Text
renderItems fmt = T.concat . map renderItem
    where
        renderItem (Syllable ts)
            -- The Unicode renderer reads the position marks (wave 3.3): a
            -- subfix prints its subjoined letter, an implicit @a@ prints
            -- nothing, a word-initial vowel gets the a-chen written out.
            -- The Wylie renderer keeps the old per-token behavior.
            | fmt == OutUnicode = renderSyllable (toList ts)
            | otherwise = T.concat (toList (fmap (renderToken fmt . snd) ts))
        renderItem (Number ts) = T.concat (map (renderToken fmt) ts)
        renderItem (Punct ts) = T.concat (map (renderToken fmt) ts)
        renderItem (Other ts) = T.concat (map (renderToken fmt) ts)

        renderSyllable ms =
            prependA ms <> T.concat (go False (toList ms))
            where
                prependA ((Vowel, tok) : _)
                    | tokenSource tok == TsWylie = "ཨ"
                prependA _ = ""

                -- A Tibetan token always prints its own slice (identity holds even
                -- when a mark would hide or join it: གཨ prints its ཨ, གྲ prints
                -- its joined ྲ).
                renderMark (_, tok) | tokenSource tok == TsUnicode = tokenRaw tok
                -- The implicit @a@ a Wylie writer always spells after a base.
                renderMark (ImplicitVowel, _) = ""
                renderMark (Subfix, tok) = subjoinedGlyph tok
                renderMark (_, tok) = renderToken OutUnicode tok

                -- The letters under a superfix print subjoined (rka -> རྐ, sgra ->
                -- སྒྲ): jsewts writes every letter after the superscript in its
                -- subjoined form, so a root below the superfix follows the same
                -- rule - when the stack reaches a vowel. A stack that never does
                -- backtracks, and the next letter is a fresh base instead (rk ->
                -- ར + ཀ).
                go :: Bool -> [(Position, Token)] -> [Text]
                go _ [] = []
                go underSuperfix (m@(Root, tok) : rest)
                    | underSuperfix && runReachesVowel rest = subjoinedGlyph tok : go False rest
                    | otherwise = renderMark m : go False rest
                go _ (m@(Superfix, _) : rest) = renderMark m : go True rest
                go _ (m : rest) = renderMark m : go False rest

                -- Whether the marks after a superfix's root reach a vowel before
                -- anything that ends the stack: only subfixes (and a caret) may
                -- stand between the root and the vowel.
                runReachesVowel :: [(Position, Token)] -> Bool
                runReachesVowel ((pos, tok) : rest) =
                    case pos of
                        Subfix -> runReachesVowel rest
                        Final
                            | tokenCanonical tok == TcFinal FMCaret -> runReachesVowel rest
                        Vowel -> True
                        ImplicitVowel -> True
                        _ -> False
                runReachesVowel [] = False

                subjoinedGlyph tok =
                    case subjoinOf tok of
                        Just sc -> fromMaybe "" (unicodeOf (TcSubConsonant sc))
                        Nothing -> renderToken OutUnicode tok

        renderToken fmt tok
            | tokenSource tok == sourceOf fmt = tokenRaw tok
            -- a token without a cross-script spelling falls back to the text it
            -- stands for: the content of a bracket block, the character of an
            -- escape, and otherwise the raw slice
            | otherwise = fromMaybe (unknownText tok) (scriptOf fmt (tokenCanonical tok))

        unknownText tok =
            case tokenCanonical tok of
                TcUnknown (UnknownMark text) -> text
                _ -> tokenRaw tok

        sourceOf OutUnicode = TsUnicode
        sourceOf OutWylie = TsWylie

        scriptOf OutUnicode = unicodeOf
        scriptOf OutWylie = wylieOf
