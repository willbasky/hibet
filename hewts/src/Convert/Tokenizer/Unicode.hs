module Convert.Tokenizer.Unicode where

import Control.Applicative (asum)
import Convert.Diagnostic
import Convert.Token
import Convert.Token.Unicode
import Data.Text (Text)
import qualified Data.Text as T

-- | Tokenize Unicode Tibetan input to typed IR tokens. Deprecated
-- precomposed characters (aspirates, long vowels, vocalic r/l) decompose
-- into a token sequence; see 'unicodeDecompose'. The syllable OM (ༀ) is
-- also a precomposed spelling - a-chen + o + anusvara - and decomposes the
-- same way, as ewts-js does.
--
-- A byte-order mark and a zero-width space are skipped without a word, the
-- way the reference skips them. The diagnostics come back with the tokens:
-- the first of this direction is a Tibetan character no table can name
-- ('unnameableGlyph'), which every reference converter escapes in Wylie
-- output.
tokenizeUnicode :: Text -> ([Token], Diagnostics)
tokenizeUnicode input = (reverse tokensRev, diagnosticsInOrder diagsRev)
    where
        (tokensRev, diagsRev) = go [] mempty (0 :: Int) input

        go accT accD _ rest
            | T.null rest = (accT, accD)
        go accT accD offset rest =
            case T.uncons rest of
                Nothing -> (accT, accD)
                Just (c, next)
                    | isSkippedChar c -> go accT accD (offset + 1) next
                    | otherwise ->
                        let end = offset + 1
                            sp = mkSpan (fromIntegral offset) (fromIntegral end)
                            toks = classifyChar sp (T.singleton c) c
                            diag = unnameableGlyphOf c toks
                         in go
                                (reverse toks <> accT)
                                (maybe accD (`addDiagnostic` accD) diag)
                                end
                                next

-- | A Tibetan character no table names (༉, ࿂): one unknown token over the
-- Tibetan plane, and the warning for it. Latin letters and everything else
-- outside the plane are simply not Tibetan - this is about a glyph Wylie
-- cannot spell, which the reference escapes as @\\uXXXX@.
unnameableGlyphOf :: Char -> [Token] -> Maybe Diagnostic
unnameableGlyphOf c toks
    | c >= '\x0f00' && c <= '\x0fff'
    , [t] <- toks
    , TcUnknown _ <- tokenCanonical t =
        Just (unnameableGlyph (tokenSpan t) (T.singleton c))
    | otherwise = Nothing

-- | The reference passes a byte-order mark and a zero-width space through
-- without a word; so do we.
isSkippedChar :: Char -> Bool
isSkippedChar c = c == '\xfeff' || c == '\x200b'

classifyChar :: Span -> Text -> Char -> [Token]
classifyChar sp raw ch =
    case canonicalSeq ch of
        Just canons -> mkSequenceTokens TsUnicode sp raw canons
        Nothing -> [mkUnknown TsUnicode sp raw]

-- | Canonical token sequence for a single character: either one canonical or
-- a decomposition into several (deprecated precomposed forms).
canonicalSeq :: Char -> Maybe [TokenCanonical]
canonicalSeq ch =
    case lookup ch unicodeDecompose of
        Just canons -> Just canons
        Nothing -> fmap pure (classifyCanonical ch)

-- | Map a single character to its shared canonical payload, via the same
-- inverse tables used by 'classifyChar'.
classifyCanonical :: Char -> Maybe TokenCanonical
classifyCanonical ch =
    asum
        [ TcConsonant <$> inverseUnicodeConsonant raw
        , TcSubConsonant <$> inverseUnicodeSubConsonant raw
        , TcVowel <$> inverseUnicodeVowel raw
        , TcFinal <$> inverseUnicodeFinal raw
        , TcNumber <$> inverseUnicodeNumber raw
        , TcHalfNumber <$> inverseUnicodeHalfNumber raw
        , TcPunctuation <$> inverseUnicodePunctuation raw
        , TcSign <$> inverseUnicodeSign raw
        , TcSanskritMark <$> inverseUnicodeSanskritMark raw
        , TcOrnament <$> inverseUnicodeOrnament raw
        , TcSymbol <$> inverseUnicodeSymbol raw
        , TcSpace <$> inverseUnicodeSpace raw
        ]
    where
        raw = T.singleton ch
