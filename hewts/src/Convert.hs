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
import Convert.Grammar.Legality (legality)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Grammar.Word (Position (..))
import Convert.Sentence (SpellItem (..), pSentence)
import Convert.Token
    ( Consonant (..)
    , FinalMark (FMCaret)
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
    pure (items, diagnostics <> legality tokens items)

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
            -- The Wylie renderer walks the same marks and writes the
            -- separators the Wylie spelling needs to stay readable back (step
            -- 5 refactor): the implicit @a@ of a vowel-less bare root, the
            -- dot before a full root that would otherwise glue to its prefix
            -- letter, and a word-initial a-chen that only carries a vowel
            -- sign.
            | fmt == OutUnicode = renderSyllable (toList ts)
            | otherwise = renderSyllableWylie (toList ts)
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

        -- Wylie output is rendered per token, with three mark-aware additions
        -- that keep the spelling readable back by the Wylie arms:
        --
        --   * the implicit @a@ of a vowel-less bare full root is written out
        --     when the root is followed by another full letter (སའི ->
        --     "sa'i", གནག -> "gnag") - the syllable's closing letter never
        --     gets it (མངའ -> "mnga'", ས -> "s");
        --   * a full root that follows a prefix or superfix letter gets a dot
        --     before it when the two letters could glue into a subjoined
        --     stack (གཡུལ -> "g.yul", བརས -> "b.ras") - གནག -> "gnag" stays
        --     compact because n never subjoins;
        --   * a word-initial a-chen that only carries a vowel sign is not
        --     written out (ཨུ -> "u"), since the vowel alone restores it.
        --
        -- Subjoined letters keep their raw glyph spelling: the Wylie
        -- tokenizer accepts the Tibetan subjoined letters as aliases, so the
        -- stacks round-trip exactly as they did before, and only the forms
        -- above gain separators.
        renderSyllableWylie marks = go Nothing marks
            where
                go _ [] = ""
                go prev (m : rest) =
                    markText prev m rest <> implicitA m rest <> go (Just m) rest

                markText _ (Root, tok) rest
                    | isAChen tok, nextIsVowel rest = ""
                markText prev (Root, tok) _
                    | isFullLetter tok
                    , needsDot prev tok =
                        "." <> renderToken OutWylie tok
                markText _ (_, tok) _ = renderToken OutWylie tok

                -- Whether the letters could form a subjoined stack: only then
                -- does a dot between a prefix letter and a following full
                -- root change how the spelling is read.
                needsDot (Just (prevPos, prevTok)) tok =
                    isFullLetter prevTok
                        && prevPos `elem` [Prefix, Superfix]
                        && isSubjoinLetter tok
                needsDot _ _ = False

                -- The implicit @a@ after a vowel-less bare full root, written
                -- only before the next full letter (a Root, Suffix, Postfix
                -- or Final mark). A syllable's closing letter never gets it
                -- (མངའ -> "mnga'", ས -> "s"): a trailing @a@ would read back
                -- as a fresh a-chen once the syllable already ran its course.
                implicitA (Root, tok) rest
                    | isFullLetter tok
                    , not (isAChen tok) =
                        case rest of
                            (nextPos, _) : _
                                | nextPos `elem` [Root, Suffix, Postfix, Final] -> "a"
                            _ -> ""
                implicitA _ _ = ""

                isFullLetter tok =
                    case tokenCanonical tok of
                        TcConsonant _ -> True
                        _ -> False

                isAChen tok = tokenCanonical tok == TcConsonant Ca

                isSubjoinLetter tok =
                    case tokenCanonical tok of
                        TcConsonant c -> c `elem` [Cy, Cr, Cl, Cw]
                        _ -> False

                nextIsVowel rest =
                    case rest of
                        (Vowel, _) : _ -> True
                        _ -> False

        renderToken ofmt tok
            | tokenSource tok == sourceOf ofmt = tokenRaw tok
            -- a token without a cross-script spelling falls back to the text it
            -- stands for: the content of a bracket block, the character of an
            -- escape, and otherwise the raw slice
            | otherwise = fromMaybe (unknownText tok) (scriptOf ofmt (tokenCanonical tok))

        unknownText tok =
            case tokenCanonical tok of
                TcUnknown (UnknownMark text) -> text
                _ -> tokenRaw tok

        sourceOf OutUnicode = TsUnicode
        sourceOf OutWylie = TsWylie

        scriptOf OutUnicode = unicodeOf
        scriptOf OutWylie = wylieOf
