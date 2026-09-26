{- HLINT ignore "Use camelCase" -}

module Convert.Tokenizer.Wylie where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, isHexDigit)
import Convert.Diagnostic
import Convert.Token
import Convert.Tokenizer.Unicode (canonicalSeq)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative (asum, (<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Trie as Trie
import Numeric (readHex)


-- special characters: flag those if they occur out of context.
-- '+' and '.' are ConSpec tokens, so they are not flagged here any more.
special :: HashSet Text
special = HS.fromList ["-", "~", "^", "?", "`", "]"]

-- Longest-match lookup over known multi-char Wylie tokens.
longTokenTrie :: Trie.Trie ()
longTokenTrie =
    Trie.fromList [(TE.encodeUtf8 tok, ()) | tok <- longTokenList]

-- | All multi-char Wylie spellings (representatives + aliases), derived from
-- the render tables to keep the trie and the tables in sync.
longTokenList :: [Text]
longTokenList =
    HS.toList . HS.fromList $
        concat
            [ [ s | s <- map wylieConsonant [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieVowel [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieFinal [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieNumber [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieHalfNumber [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wyliePunctuation [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieSign [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieSanskritMark [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieOrnament [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieSymbol [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieSpace [minBound .. maxBound], T.length s > 1 ]
            , map fst wylieConsonantAliases
            , map fst wylieExpansions
            , ["b+l", "\r\n"]
            ]

-- | Tokenize Wylie input using longest-match splitting for known multi-char
-- tokens. One Wylie spelling may produce several tokens (compound forms such
-- as aspirates and long vowels): see 'wylieExpansions'.
--
-- Alongside the tokens, the diagnostics collected on the way are returned:
-- BOM and ZWSP are skipped silently, line breaks collapse, brackets mark
-- foreign text, and characters that occur where nothing expects them are
-- reported. Only the token layer knows positions; line numbers are worked out
-- later, in 'Convert.Diagnostic'.
tokenizeWylie :: Text -> ([Token], Diagnostics)
tokenizeWylie input = (reverse tokensRev, diagnosticsInOrder diagsRev)
  where
    (tokensRev, diagsRev) = go [] mempty 0 input

    go accT accD _ rest
        | T.null rest = (accT, accD)
    go accT accD offset rest
        | isSkipped rest = go accT accD (offset + 1) (T.drop 1 rest)
        | brk > 0 = go accT accD (offset + brk) (T.drop brk rest)
        | otherwise =
            let (chunk, next, commentClosed) = nextChunk rest
                end = offset + T.length chunk
                sp = mkSpan (fromIntegral offset) (fromIntegral end)
                (toks, diags) = classifyTokens sp chunk commentClosed
             in go (reverse toks <> accT) (diags <> accD) end next
      where
        brk = lineBreakLen rest

-- | The reference skips a byte-order mark and a zero-width space without
-- saying anything about them.
isSkipped :: Text -> Bool
isSkipped txt =
    case T.uncons txt of
        Just (c, _) -> c == '\xfeff' || c == '\x200b'
        Nothing -> False

-- | Line breaks end a line but produce nothing: like the reference, runs of
-- whitespace (and any line break) collapse instead of reaching the output.
lineBreakLen :: Text -> Int
lineBreakLen txt
    | T.isPrefixOf "\r\n" txt = 2
    | T.isPrefixOf "\n" txt = 1
    | T.isPrefixOf "\r" txt = 1
    | otherwise = 0

-- | One lexical chunk, the rest of the input, and for a bracketed block
-- whether the closing bracket was found.
nextChunk :: Text -> (Text, Text, Maybe Bool)
nextChunk source
    | T.null source = (T.empty, T.empty, Nothing)
    | T.isPrefixOf "[" source =
        let (chunk, next, closed) = consumeBracketed source
         in (chunk, next, Just closed)
    | T.isPrefixOf "\\" source =
        let (chunk, next) = consumeEscape source
         in (chunk, next, Nothing)
    | otherwise =
        case Trie.match longTokenTrie sourceBytes of
            Just (prefix, _, rest)
                | not (BS.null prefix) ->
                    (TE.decodeUtf8 prefix, TE.decodeUtf8 rest, Nothing)
            _ -> (T.take 1 source, T.drop 1 source, Nothing)
  where
    sourceBytes :: ByteString
    sourceBytes = TE.encodeUtf8 source

consumeBracketed :: Text -> (Text, Text, Bool)
consumeBracketed txt =
    case closeAt 1 1 False of
        Just end -> (T.take end txt, T.drop end txt, True)
        Nothing -> (txt, T.empty, False)
  where
    txtLen = T.length txt

    closeAt :: Int -> Int -> Bool -> Maybe Int
    closeAt i depth escaped
        | i >= txtLen = Nothing
        | escaped = closeAt (i + 1) depth False
        | otherwise =
            case T.index txt i of
                '\\' -> closeAt (i + 1) depth True
                '[' -> closeAt (i + 1) (depth + 1) False
                ']' ->
                    if depth == 1
                        then Just (i + 1)
                        else closeAt (i + 1) (depth - 1) False
                _ -> closeAt (i + 1) depth False

-- | One escape chunk. A \\uXXXX / \\UXXXXXXXX sequence is swallowed whole even
-- when its digits are not hexadecimal, so that the invalid one can be quoted
-- back in a warning; anything else is a backslash plus one character.
consumeEscape :: Text -> (Text, Text)
consumeEscape txt
    | T.length txt < 2 = (txt, T.empty)
    | T.isPrefixOf "\\u" txt && T.length txt >= 6 = (T.take 6 txt, T.drop 6 txt)
    | T.isPrefixOf "\\U" txt && T.length txt >= 10 = (T.take 10 txt, T.drop 10 txt)
    | otherwise = (T.take 2 txt, T.drop 2 txt)

-- | Classify one lexical chunk. Returns the tokens plus whatever was noticed
-- on the way: a stray letter or special marker, an unclosed bracket, a broken
-- escape.
classifyTokens :: Span -> Text -> Maybe Bool -> ([Token], Diagnostics)
classifyTokens sp raw commentClosed
    | raw == "b+l" = ([mkUnknown TsWylie sp raw], mempty)
    | raw == "+" = ([mkToken TsWylie TkConSpec raw (TcConSpec CSPlus) sp], mempty)
    | raw == "." = ([mkToken TsWylie TkConSpec raw (TcConSpec CSDot) sp], mempty)
    | Just closed <- commentClosed = nonTibetanComment closed
    | isEscapeChunk raw = decodeEscape sp raw
    | otherwise = classifyPlain
  where
    -- a bracketed block of foreign text: one token covering the whole block,
    -- plus a warning when the closing bracket never arrives
    nonTibetanComment closed =
        ( [mkNonTibetan TsWylie sp raw (commentText closed raw)]
        , if closed then mempty else addDiagnostic (unfinishedComment sp) mempty
        )

    classifyPlain =
        case lookup raw wylieExpansions of
            Just canons -> (mkSequenceTokens TsWylie sp raw canons, mempty)
            Nothing ->
                case lookupTable raw of
                    Just tok -> ([tok], mempty)
                    Nothing
                        | isSpecial raw -> ([specialToken], unexpected)
                        | otherwise -> ([mkUnknown TsWylie sp raw], unexpected)

    specialToken =
        mkUnknownWith
            TsWylie
            sp
            raw
            [TokenIssue InvalidSequence TisWarning "Special marker out of context"]

    -- the reference reports a bare ASCII letter or a special marker that
    -- occurs where nothing expects it; anything else (a quotation mark, a
    -- foreign letter) passes through without a word
    unexpected
        | needsReport = addDiagnostic (unexpectedCharacter sp c) mempty
        | otherwise = mempty
      where
        Just (c, _) = T.uncons raw
        needsReport = isAsciiLetter c || HS.member raw special

    isAsciiLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    lookupTable key = lookupSpelling sp raw key

    isSpecial key = HS.member key special

    -- an escape of the right shape; whether its code is valid hex is decided
    -- in 'decodeEscape', which reports it when it is not
    isEscapeChunk chunk
        | T.length chunk == 2 && T.head chunk == '\\' = True
        | T.length chunk == 6 && T.isPrefixOf "\\u" chunk = True
        | T.length chunk == 10 && T.isPrefixOf "\\U" chunk = True
        | otherwise = False

-- | Look a Wylie spelling up in the render tables. The token's raw slice is
-- always the whole chunk, which may be a different spelling of the key (an
-- escape, for instance), so the key and the raw are separate arguments.
lookupSpelling :: Span -> Text -> Text -> Maybe Token
lookupSpelling sp raw key =
    asum
        [ mkConsonant TsWylie sp raw <$> withAliases inverseWylieConsonant key
        , mkVowel TsWylie sp raw <$> inverseWylieVowel key
        , mkFinal TsWylie sp raw <$> inverseWylieFinal key
        , mkNumber TsWylie sp raw <$> inverseWylieNumber key
        , mkPunctuation TsWylie sp raw <$> inverseWyliePunctuation key
        , mkSymbol TsWylie sp raw <$> inverseWylieSymbol key
        , mkSpace TsWylie sp raw <$> inverseWylieSpace key
        ]
  where
    withAliases inverseLookup x = inverseLookup x <|> lookup x wylieConsonantAliases

-- | Render a canonical token to its Wylie spelling.
wylieOf :: TokenCanonical -> Maybe Text
wylieOf = \case
    TcConsonant c -> Just (wylieConsonant c)
    TcSubConsonant _ -> Nothing
    TcVowel v -> Just (wylieVowel v)
    TcFinal f -> Just (wylieFinal f)
    TcNumber n -> Just (wylieNumber n)
    TcHalfNumber h -> Just (wylieHalfNumber h)
    TcPunctuation p -> Just (wyliePunctuation p)
    TcSign s -> Just (wylieSign s)
    TcSanskritMark m -> Just (wylieSanskritMark m)
    TcOrnament o -> Just (wylieOrnament o)
    TcSymbol s -> Just (wylieSymbol s)
    TcSpace m -> Just (wylieSpace m)
    TcConSpec cs -> Just (wylieConSpec cs)
    _ -> Nothing

-- | Wylie spelling of a ConSpec operator.
wylieConSpec :: ConSpec -> Text
wylieConSpec = \case
    CSPlus -> "+"
    CSDot -> "."

-- | Decode a \\uXXXX or \\UXXXXXXXX escape chunk. The named character may
-- map to several canonical tokens (deprecated precomposed forms are
-- decomposed here too), and a code point that is not a known character stays
-- opaque - the reference re-emits the escape rather than the character.
--
-- An escape that cannot be a code point at all is neither: the reference
-- reports it and drops it, and so do we.
decodeEscape :: Span -> Text -> ([Token], Diagnostics)
decodeEscape sp raw
    | isBrokenHexEscape raw = ([], addDiagnostic (invalidHexCode sp raw) mempty)
    | otherwise =
        case decodeHexCode raw of
            Just c ->
                ( maybe
                    [mkUnknownWith TsWylie sp raw []]
                    (mkSequenceTokens TsWylie sp raw)
                    (canonicalSeq c)
                , mempty
                )
            -- "\3" and friends: the escaped character stands for itself, while
            -- the raw slice stays the whole escape
            Nothing -> ([escapedCharacter], mempty)
  where
    escapedCharacter =
        case T.uncons (T.drop 1 raw) of
            Nothing -> mkUnknownWith TsWylie sp raw []
            Just (c, _) ->
                fromMaybe
                    (mkToken TsWylie TkUnknown raw (TcUnknown (UnknownMark (T.singleton c))) sp)
                    (lookupSpelling sp raw (T.singleton c))

-- | A \\uXXXX / \\UXXXXXXXX escape whose digits are not all hexadecimal: the
-- reference's tokenizer still swallows the whole sequence, so the message can
-- quote it.
isBrokenHexEscape :: Text -> Bool
isBrokenHexEscape raw
    | T.length raw == 6 && T.isPrefixOf "\\u" raw = not (T.all isHexDigit (T.drop 2 raw))
    | T.length raw == 10 && T.isPrefixOf "\\U" raw = not (T.all isHexDigit (T.drop 2 raw))
    | otherwise = False

decodeHexCode :: Text -> Maybe Char
decodeHexCode raw
    | T.isPrefixOf "\\u" raw && T.length raw == 6 = readHexCode (T.drop 2 raw)
    | T.isPrefixOf "\\U" raw && T.length raw == 10 = readHexCode (T.drop 2 raw)
    | otherwise = Nothing
  where
    readHexCode hex =
        case readHex (T.unpack hex) of
            [(n, "")] | n <= 0x10FFFF -> Just (chr n)
            _ -> Nothing

-- | The content of a bracketed block of foreign text, which is what reaches
-- the output: the outer brackets are Wylie-only syntax, and escapes inside are
-- decoded. An unclosed block keeps its inner brackets, as the reference does -
-- so the flag has to be passed in rather than guessed from the last character.
commentText :: Bool -> Text -> Text
commentText closed raw = T.pack (go body)
  where
    body
        | closed = T.dropEnd 1 (T.drop 1 raw)
        | otherwise = T.drop 1 raw
    go t
        | T.null t = []
        | T.head t == '\\' && isEscape t =
            maybe id (:) (escapedChar t) (go (T.drop (escapeLen t) t))
        | otherwise =
            case T.uncons t of
                Just (c, rest) -> c : go rest
                Nothing -> []
    isEscape t = escapeLen t /= 1

-- | The character a Wylie escape stands for: a hex escape decodes to its code
-- point, a backslash before anything else stands for that character itself.
escapedChar :: Text -> Maybe Char
escapedChar t
    | isHexEscape t = codePoint t
    | T.length t >= 2 = fmap fst (T.uncons (T.drop 1 t))
    | otherwise = Nothing

isHexEscape :: Text -> Bool
isHexEscape t = escapeLen t /= 2 && T.length t >= 2

escapeLen :: Text -> Int
escapeLen t
    | T.isPrefixOf "\\u" t && T.length t >= 6 && T.all isHexDigit (T.take 4 (T.drop 2 t)) = 6
    | T.isPrefixOf "\\U" t && T.length t >= 10 && T.all isHexDigit (T.take 8 (T.drop 2 t)) = 10
    | T.length t >= 2 = 2
    | otherwise = 1

codePoint :: Text -> Maybe Char
codePoint t =
    case readHex (T.unpack (T.take (len - 2) (T.drop 2 t))) of
        [(n, "")] | n <= 0x10FFFF -> Just (chr n)
        _ -> Nothing
  where
    len = escapeLen t

wylieConsonant :: Consonant -> Text
wylieConsonant = \case
    Ck -> "k"
    Ckh -> "kh"
    Cg -> "g"
    Cng -> "ng"
    Cc -> "c"
    Cch -> "ch"
    Cj -> "j"
    Cny -> "ny"
    CT -> "T"
    CTh -> "Th"
    CD -> "D"
    CN -> "N"
    Ct -> "t"
    Cth -> "th"
    Cd -> "d"
    Cn -> "n"
    Cp -> "p"
    Cph -> "ph"
    Cb -> "b"
    Cm -> "m"
    Cts -> "ts"
    Ctsh -> "tsh"
    Cdz -> "dz"
    Cw -> "w"
    Czh -> "zh"
    Cz -> "z"
    C' -> "'"
    Cy -> "y"
    Cr -> "r"
    Cl -> "l"
    Csh -> "sh"
    CSh -> "Sh"
    Cs -> "s"
    Ch -> "h"
    Ca -> "a"
    CR -> "R"
    Ckka -> "\\u0f6b"
    CRra -> "\\u0f6c"

wylieVowel :: Vowel -> Text
wylieVowel = \case
    VA -> "A"
    Vi -> "i"
    Vu -> "u"
    Ve -> "e"
    Vai -> "ai"
    Vo -> "o"
    Vau -> "au"
    V_i -> "-i"

wylieFinal :: FinalMark -> Text
wylieFinal = \case
    FMAnusvara -> "M"
    FMBinduNada -> "~M`"
    FMCandrabindu -> "~M"
    FMSrogMed -> "X"
    FMCandrabinduHalanta -> "~X"
    FMVisarga -> "H"
    FMHalanta -> "?"
    FMCaret -> "^"
    FMYigMgo -> "&"

wylieNumber :: Number -> Text
wylieNumber = \case
    N0 -> "0"
    N1 -> "1"
    N2 -> "2"
    N3 -> "3"
    N4 -> "4"
    N5 -> "5"
    N6 -> "6"
    N7 -> "7"
    N8 -> "8"
    N9 -> "9"

wylieHalfNumber :: HalfNumber -> Text
wylieHalfNumber = \case
    H_0 -> "\\u0f33"
    H_1 -> "\\u0f2a"
    H_2 -> "\\u0f2b"
    H_3 -> "\\u0f2c"
    H_4 -> "\\u0f2d"
    H_5 -> "\\u0f2e"
    H_6 -> "\\u0f2f"
    H_7 -> "\\u0f30"
    H_8 -> "\\u0f31"
    H_9 -> "\\u0f32"

wyliePunctuation :: PunctuationMark -> Text
wyliePunctuation = \case
    PMTsheg -> " "
    PMNonBreakingTsheg -> "*"
    PMShad -> "/"
    PMNyisShad -> "//"
    PMTshegShad -> ";"
    PMNyisTshegShad -> "\\u0f10"
    PMRinChenSpungsShad -> "|"
    PMRgyaGramShad -> "\\u0f12"
    PMCaretDzudRtagsMeLong -> "\\u0f13"
    PMGterTshigMgo -> ":"

wylieSign :: SignMark -> Text
wylieSign = \case
    SGYigMgoAt -> "\\u0f00"
    SGKaKhaGaGsum -> "\\u0f01"
    SGNyiZlaNaaDa -> "\\u0f02"
    SGSbrulShad -> "\\u0f03"

wylieSanskritMark :: SanskritMark -> Text
wylieSanskritMark = \case
    SMiLciRtags -> "\\u0f86"
    SMiYangRtags -> "\\u0f87"
    SMiLceTsaCanSubjoined -> "\\u0f8d"
    SMiMchuCanSubjoined -> "\\u0f8e"
    SMiInvertedMchuCanSubjoined -> "\\u0f8f"

wylieOrnament :: OrnamentMark -> Text
wylieOrnament = \case
    OMRdelDkarGcig -> "\\u0fd0"
    OMRdelDkarGnyis -> "\\u0fd1"
    OMRdelDkarGsum -> "\\u0fd2"
    OMRdelNagGcig -> "\\u0fd3"
    OMRdelNagGnyis -> "\\u0fd4"
    OMLeadingMchanRtags -> "\\u0fd9"
    OMTrailingMchanRtags -> "\\u0fda"

wylieSymbol :: SymbolMark -> Text
wylieSymbol = \case
    SMExclamation -> "!"
    SMAt -> "@"
    SMHash -> "#"
    SMDollar -> "$"
    SMPercent -> "%"
    SMEqual -> "="
    SMLt -> "<"
    SMGt -> ">"
    SMLParen -> "("
    SMRParen -> ")"

wylieSpace :: SpaceMark -> Text
wylieSpace = \case
    SMSpace -> "_"

wylieConsonantAliases :: [(Text, Consonant)]
wylieConsonantAliases =
    [ ("-t", CT)
    , ("-th", CTh)
    , ("-d", CD)
    , ("-n", CN)
    , ("W", Cw)
    , ("Y", Cy)
    , ("-sh", CSh)
    ]

-- | Wylie spellings that expand to several canonical tokens.
--
-- These are the compound forms of the canonical domain: aspirated consonants
-- ("gh" = g + subjoined-h), long vowels ("I" = A+i, "E" = A+e, ...),
-- vocalic r/l ("r-i" = subjoined-r + reverse-i) and the EWTS letters
-- f/v ("f" = ph + caret, "v" = b + caret). No precomposed canonical
-- exists for them, so a single spelling produces a token sequence.
wylieExpansions :: [(Text, [TokenCanonical])]
wylieExpansions =
    [ ("gh", [TcConsonant Cg, TcSubConsonant SCh])
    , ("g+h", [TcConsonant Cg, TcSubConsonant SCh])
    , ("Dh", [TcConsonant CD, TcSubConsonant SCh])
    , ("D+h", [TcConsonant CD, TcSubConsonant SCh])
    , ("dh", [TcConsonant Cd, TcSubConsonant SCh])
    , ("d+h", [TcConsonant Cd, TcSubConsonant SCh])
    , ("bh", [TcConsonant Cb, TcSubConsonant SCh])
    , ("b+h", [TcConsonant Cb, TcSubConsonant SCh])
    , ("dzh", [TcConsonant Cdz, TcSubConsonant SCh])
    , ("dz+h", [TcConsonant Cdz, TcSubConsonant SCh])
    , ("k+Sh", [TcConsonant Ck, TcSubConsonant SCSh])
    , ("f", [TcConsonant Cph, TcFinal FMCaret])
    , ("v", [TcConsonant Cb, TcFinal FMCaret])
    , ("-dh", [TcSubConsonant SCD, TcSubConsonant SCh])
    , ("-d+h", [TcSubConsonant SCD, TcSubConsonant SCh])
    , ("I", [TcVowel VA, TcVowel Vi])
    , ("U", [TcVowel VA, TcVowel Vu])
    , ("E", [TcVowel VA, TcVowel Ve])
    , ("O", [TcVowel VA, TcVowel Vo])
    , ("-I", [TcVowel VA, TcVowel V_i])
    , ("r-i", [TcSubConsonant SCr, TcVowel V_i])
    , ("r-I", [TcSubConsonant SCr, TcVowel VA, TcVowel V_i])
    , ("l-i", [TcSubConsonant SCl, TcVowel V_i])
    , ("l-I", [TcSubConsonant SCl, TcVowel VA, TcVowel V_i])
    ]

inverseWylieConsonant :: Text -> Maybe Consonant
inverseWylieConsonant = inverseMap wylieConsonant

inverseWylieVowel :: Text -> Maybe Vowel
inverseWylieVowel = inverseMap wylieVowel

inverseWylieFinal :: Text -> Maybe FinalMark
inverseWylieFinal = inverseMap wylieFinal

inverseWylieNumber :: Text -> Maybe Number
inverseWylieNumber = inverseMap wylieNumber

inverseWyliePunctuation :: Text -> Maybe PunctuationMark
inverseWyliePunctuation = inverseMap wyliePunctuation

inverseWylieSymbol :: Text -> Maybe SymbolMark
inverseWylieSymbol = inverseMap wylieSymbol

inverseWylieSpace :: Text -> Maybe SpaceMark
inverseWylieSpace = inverseMap wylieSpace