{- HLINT ignore "Use camelCase" -}

module Convert.Tokenizer.Wylie where

import Control.Applicative (asum, (<|>))
import Convert.Diagnostic
import Convert.Token
import Convert.Token.Wylie
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, isHexDigit, isSpace)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
-- the alphabet tables in 'Convert.Token.Wylie' to keep the trie and the
-- tables in sync.
longTokenList :: [Text]
longTokenList =
    HS.toList . HS.fromList $
        concat
            [ [s | s <- map wylieConsonant [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieVowel [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieFinal [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieNumber [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieHalfNumber [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wyliePunctuation [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieSign [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieSanskritMark [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieOrnament [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieSymbol [minBound .. maxBound], T.length s > 1]
            , [s | s <- map wylieSpace [minBound .. maxBound], T.length s > 1]
            , map fst wylieConsonantAliases
            , map fst wylieRaisedSubjoined
            , map fst wylieExpansions
            , ["b+l", "\r\n"]
            ]

-- | Sloppy Wylie: the way people actually type it. Typographic apostrophes,
-- capitals in the middle of a word, a lone @b@ or @m@ where @ba@\/@ma@ was
-- meant, @adm@ for a stacked @d+m@. The corrections are the reference's
-- @normalizeSloppyWylie@, applied in its order: @H @ loses its space before
-- the smart lowercase looks at what stands before @H@, and @Sh@ is protected
-- only after @H@ has been lowered.
--
-- One reference pair is dropped: @x@\/@X@ -> U+0FBE. It would turn the EWTS
-- final @X@ (U+0F37, seen as @~Xa@ in the golden snapshot) into another sign,
-- and the reference's own JS port (jsewts, @sloppyRepl@) does not do it either.
normalizeSloppy :: Text -> Text
normalizeSloppy input =
    protectSh
        (lowerFinalM (lowerFinalH (foldl' replaceOne input sloppyReplacements)))
    where
        replaceOne text (from, to) = T.replace from to text

-- | The reference's pairs, in its order (later rules rely on earlier ones).
sloppyReplacements :: [(Text, Text)]
sloppyReplacements =
    [ ("ʼ", "'")
    , ("ʹ", "'")
    , ("‘", "'")
    , ("’", "'")
    , ("ʾ", "'")
    , ("...", "\x0f0b\x0f0b\x0f0b")
    , (" (", "_(")
    , (") ", ")_")
    , ("/ ", "/_")
    , (" 0", "_0")
    , (" 1", "_1")
    , (" 2", "_2")
    , (" 3", "_3")
    , (" 4", "_4")
    , (" 5", "_5")
    , (" 6", "_6")
    , (" 7", "_7")
    , (" 8", "_8")
    , (" 9", "_9")
    , ("_ ", "__")
    , ("G", "g")
    , ("K", "k")
    , ("C", "c")
    , ("B", "b")
    , (" b ", " ba ")
    , ("Ts", "ts")
    , ("Dz", "dz")
    , ("Ny", "ny")
    , ("Ng", "ng")
    , (" m ", " ma ")
    , (" m'i ", " ma'i ")
    , (" b'i ", " ba'i ")
    , ("P", "p")
    , ("L", "l")
    , ("Z", "z")
    , ("J", "j")
    , ("（", "(")
    , ("）", ")")
    , ("༼", "(")
    , ("༽", ")")
    , ("：", ":")
    , ("H ", "H")
    , ("adm", "ad+m")
    ]

-- | A final @H@ lowers unless a vowel stands before it: after a consonant it
-- is the lazy spelling of @h@. At the start of the input it has no vowel
-- before it, so it lowers too (the reference's @^@ branch).
lowerFinalH :: Text -> Text
lowerFinalH = lowerFinal "aeiouAIU" 'H' 'h'

-- | The same for @M@, with @~@ also counted as a keeper: @~M@ is the
-- candrabindu, not a lazy @m@.
lowerFinalM :: Text -> Text
lowerFinalM = lowerFinal "aeiouAIU~" 'M' 'm'

lowerFinal :: String -> Char -> Char -> Text -> Text
lowerFinal vowels upper lower = T.pack . go Nothing . T.unpack
    where
        go _ [] = []
        go prev (c : cs)
            | c == upper && not (afterVowel prev) = lower : go prev cs
            | otherwise = c : go (Just c) cs
        afterVowel = maybe False (`elem` vowels)

-- | @S@ lowers, except in @Sh@ (the retroflex @sha@). Splitting on the
-- protected pair avoids a placeholder that the input could contain itself.
protectSh :: Text -> Text
protectSh = T.intercalate "Sh" . map (T.replace "S" "s") . T.splitOn "Sh"

-- | Tokenize Wylie input using longest-match splitting for known multi-char
-- tokens. One Wylie spelling may produce several tokens (compound forms such
-- as aspirates and long vowels): see 'wylieExpansions'.
--
-- Alongside the tokens, the diagnostics collected on the way are returned:
-- BOM and ZWSP are skipped silently, a line break stands as written (the
-- spaces behind it are eaten, as the reference's fix_spacing eats them),
-- brackets mark foreign text, and characters that occur where nothing expects
-- them are reported. Only the token layer knows positions; line numbers are
-- worked out later, in 'Convert.Diagnostic'.
--
-- Leading whitespace of the whole input is dropped, as the reference does when
-- fixing spacing (@^\s+@): a document starts at its first real letter, so an
-- indented line does not open with a tsheg. A run of spaces anywhere else is
-- one tsheg carrying the whole run (see 'nextChunk'): the reference collapses
-- the run instead of writing one tsheg per space.
tokenizeWylie :: Text -> ([Token], Diagnostics)
tokenizeWylie input = (reverse tokensRev, diagnosticsInOrder diagsRev)
    where
        (tokensRev, diagsRev) = go [] mempty 0 (T.dropWhile isSpace input)

        go accT accD _ rest
            | T.null rest = (accT, accD)
        go accT accD offset rest
            | isSkipped rest = go accT accD (offset + 1) (T.drop 1 rest)
            | brk > 0 =
                -- A line break stands as it was written: both references keep
                -- it in the Tibetan output (jsewts, ewts-js) and print it as
                -- itself on the way back. The spaces behind it are eaten, as
                -- the reference's fix_spacing eats them, so one line break
                -- never leaves two tshegs behind; a space *before* the break
                -- has already spelled its one tsheg.
                let sp = mkSpan (fromIntegral offset) (fromIntegral (offset + brk))
                    tok = mkUnknown TsWylie sp (T.take brk rest)
                    after = T.drop brk rest
                    eaten = T.length (T.takeWhile (== ' ') after)
                 in go (tok : accT) accD (offset + brk + eaten) (T.drop eaten after)
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

-- | Line breaks end a line and stand as written: the reference keeps them in
-- its Tibetan output and prints them as themselves on the way back, so a
-- wrapped line stays a wrapped line instead of turning into a tsheg.
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
    | T.isPrefixOf " " source =
        -- A run of spaces is one chunk: the whole run becomes a single tsheg
        -- (see 'classifyTokens'), the way the reference collapses it.
        let (run, next) = T.span (== ' ') source
         in (run, next, Nothing)
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
    -- A run of spaces spells one tsheg. The whole run stays in the raw slice
    -- (the input remains covered) and the grammar sees a single boundary, so
    -- "a     " reads as ཨ་ and "bla   ma   " as བླ་མ་ rather than one tsheg per
    -- space.
    | isSpaceRun = ([mkPunctuation TsWylie sp raw PMTsheg], mempty)
    | Just closed <- commentClosed = nonTibetanComment closed
    | isEscapeChunk raw = decodeEscape sp raw
    | otherwise = classifyPlain
    where
        isSpaceRun = not (T.null raw) && T.all (== ' ') raw

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
                            | isSpecial raw -> ([mkUnknown TsWylie sp raw], unexpected)
                            | otherwise -> ([mkUnknown TsWylie sp raw], unexpected)

        -- the reference reports a bare ASCII letter or a special marker that
        -- occurs where nothing expects it; anything else (a quotation mark, a
        -- foreign letter) passes through without a word
        unexpected
            | Just (c, _) <- T.uncons raw
            , isAsciiLetter c || HS.member raw special =
                addDiagnostic (unexpectedCharacter sp c) mempty
            | otherwise = mempty

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

-- | Look a Wylie spelling up in the alphabet tables. The token's raw slice is
-- always the whole chunk, which may be a different spelling of the key (an
-- escape, for instance), so the key and the raw are separate arguments.
lookupSpelling :: Span -> Text -> Text -> Maybe Token
lookupSpelling sp raw key =
    asum
        [ mkConsonant TsWylie sp raw <$> withAliases inverseWylieConsonant key
        , mkSubConsonant TsWylie sp raw <$> lookup key wylieRaisedSubjoined
        , mkVowel TsWylie sp raw <$> inverseWylieVowel key
        , mkFinal TsWylie sp raw <$> inverseWylieFinal key
        , mkNumber TsWylie sp raw <$> inverseWylieNumber key
        , mkPunctuation TsWylie sp raw <$> inverseWyliePunctuation key
        , mkSymbol TsWylie sp raw <$> inverseWylieSymbol key
        , mkSpace TsWylie sp raw <$> inverseWylieSpace key
        ]
    where
        withAliases inverseLookup x = inverseLookup x <|> lookup x wylieConsonantAliases

-- | Decode a \\uXXXX or \\UXXXXXXXX escape chunk, or a @\\x@ one. An escape
-- means "insert this character literally": a hex escape stands for the code
-- point it names, a backslash before anything else for the character after it
-- (@\\3@ inserts @3@). The character is not looked up in the spelling tables -
-- a deprecated precomposed form stays whole (@\\u0f77@ prints U+0F77, not its
-- decomposition) and a code point with no canonical token prints its
-- character, not the escape - because an escape names exactly the character
-- the reference appends, and nothing else.
--
-- An escape that cannot be a code point at all is neither: the reference
-- reports it and prints nothing for it. The slice still stands in the text
-- though - it separates the letters around it (@a\01x3a@ reads as two runs,
-- not one word) - so the token stays, with empty content that renders as
-- nothing.
decodeEscape :: Span -> Text -> ([Token], Diagnostics)
decodeEscape sp raw
    | isBrokenHexEscape raw =
        ( [mkToken TsWylie TkUnknown raw (TcUnknown (UnknownMark "")) sp]
        , addDiagnostic (invalidHexCode sp raw) mempty
        )
    | otherwise = ([literalCharacter], mempty)
    where
        -- The character the escape stands for: the code point of a hex escape,
        -- or the character after the backslash. The token's raw slice stays the
        -- whole escape, so the letters around it are still covered.
        literalCharacter =
            case decodeHexCode raw of
                Just c -> literal c
                Nothing -> case T.uncons (T.drop 1 raw) of
                    Just (c, _) -> literal c
                    Nothing -> mkUnknown TsWylie sp raw

        literal c =
            mkToken TsWylie TkUnknown raw (TcUnknown (UnknownMark (T.singleton c))) sp

-- | A \\uXXXX / \\UXXXXXXXX escape whose digits are not all hexadecimal: the
-- reference's tokenizer still swallows the whole sequence, so the message can
-- quote it.
isBrokenHexEscape :: Text -> Bool
isBrokenHexEscape raw
    | T.length raw == 6 && T.isPrefixOf "\\u" raw =
        not (T.all isHexDigit (T.drop 2 raw))
    | T.length raw == 10 && T.isPrefixOf "\\U" raw =
        not (T.all isHexDigit (T.drop 2 raw))
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
    | T.isPrefixOf "\\u" t
        && T.length t >= 6
        && T.all isHexDigit (T.take 4 (T.drop 2 t)) =
        6
    | T.isPrefixOf "\\U" t
        && T.length t >= 10
        && T.all isHexDigit (T.take 8 (T.drop 2 t)) =
        10
    | T.length t >= 2 = 2
    | otherwise = 1

codePoint :: Text -> Maybe Char
codePoint t =
    case readHex (T.unpack (T.take (len - 2) (T.drop 2 t))) of
        [(n, "")] | n <= 0x10FFFF -> Just (chr n)
        _ -> Nothing
    where
        len = escapeLen t
