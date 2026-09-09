{- HLINT ignore "Use camelCase" -}

module Convert.Tokenizer.Wylie where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Convert.Token
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative (asum, (<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Trie as Trie


-- special characters: flag those if they occur out of context
special :: HashSet Text
special = HS.fromList [".", "+", "-", "~", "^", "?", "`", "]"]

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
            , [ s | s <- map wyliePunctuation [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieSymbol [minBound .. maxBound], T.length s > 1 ]
            , [ s | s <- map wylieSpace [minBound .. maxBound], T.length s > 1 ]
            , map fst wylieConsonantAliases
            , map fst wylieVowelAliases
            , map fst wylieFinalAliases
            , ["b+l", "\r\n"]
            ]

-- | Tokenize Wylie input using longest-match splitting for known multi-char
-- tokens.
tokenizeWylie :: Text -> [Token]
tokenizeWylie input = go 0 input
  where
    go _ rest | T.null rest = []
    go offset rest =
        let (chunk, next) = nextChunk rest
            raw = chunk
            end = offset + T.length chunk
            span = mkSpan (fromIntegral offset) (fromIntegral end)
         in classifyToken span raw : go end next

nextChunk :: Text -> (Text, Text)
nextChunk source
    | T.null source = (T.empty, T.empty)
    | T.isPrefixOf "[" source = consumeBracketed source
    | T.isPrefixOf "\\" source = consumeEscape source
    | otherwise =
        case Trie.match longTokenTrie sourceBytes of
            Just (prefix, _, rest)
                | not (BS.null prefix) ->
                    (TE.decodeUtf8 prefix, TE.decodeUtf8 rest)
            _ -> (T.take 1 source, T.drop 1 source)
  where
    sourceBytes :: ByteString
    sourceBytes = TE.encodeUtf8 source

consumeBracketed :: Text -> (Text, Text)
consumeBracketed txt =
    case closeAt 1 1 False of
        Just end -> (T.take end txt, T.drop end txt)
        Nothing -> (txt, T.empty)
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

consumeEscape :: Text -> (Text, Text)
consumeEscape txt
    | T.length txt < 2 = (txt, T.empty)
    | T.isPrefixOf "\\u" txt
        && T.length txt >= 6
        && T.all isHexDigit (T.take 4 (T.drop 2 txt)) = (T.take 6 txt, T.drop 6 txt)
    | T.isPrefixOf "\\U" txt
        && T.length txt >= 10
        && T.all isHexDigit (T.take 8 (T.drop 2 txt)) = (T.take 10 txt, T.drop 10 txt)
    | otherwise = (T.take 2 txt, T.drop 2 txt)

classifyToken :: Span -> Text -> Token
classifyToken span raw
    | raw == "\r\n" = mkSpace TsWylie span raw SMSpace
    | raw == "b+l" = mkUnknown TsWylie span raw
    | isClosedBracketChunk raw || isKnownEscapeChunk raw = mkUnknownWith TsWylie span raw []
    | otherwise =
        fromMaybe (mkUnknown TsWylie span raw) $ asum
            [ lookupAs mkConsonant (lookupWylie inverseWylieConsonant wylieConsonantAliases)
            , lookupAs mkVowel (lookupWylie inverseWylieVowel wylieVowelAliases)
            , lookupAs mkFinal (lookupWylie inverseWylieFinal wylieFinalAliases)
            , lookupAs mkNumber inverseWylieNumber
            , lookupAs mkPunctuation inverseWyliePunctuation
            , lookupAs mkSymbol inverseWylieSymbol
            , lookupAs mkSpace inverseWylieSpace
            , if HS.member raw special
                then Just $
                    mkUnknownWith
                        TsWylie
                        span
                        raw
                        [TokenIssue InvalidSequence TisWarning "Special marker out of context"]
                else Nothing
            ]
  where
    lookupAs constructor lookupFn =
        constructor TsWylie span raw <$> lookupFn raw

    lookupWylie inverseLookup aliases x =
        inverseLookup x <|> lookup x aliases

    isClosedBracketChunk chunk =
        T.length chunk >= 2 && T.head chunk == '[' && T.last chunk == ']'

    isKnownEscapeChunk chunk
        | T.length chunk == 2 && T.head chunk == '\\' = True
        | T.length chunk == 6 && T.isPrefixOf "\\u" chunk = T.all isHexDigit (T.drop 2 chunk)
        | T.length chunk == 10 && T.isPrefixOf "\\U" chunk = T.all isHexDigit (T.drop 2 chunk)
        | otherwise = False

-- | Render a canonical token to its Wylie spelling.
wylieOf :: TokenCanonical -> Maybe Text
wylieOf = \case
    TcConsonant Ckka -> Nothing
    TcConsonant CRra -> Nothing
    TcConsonant c -> Just (wylieConsonant c)
    TcVowel Vr_i -> Nothing
    TcVowel Vr_I -> Nothing
    TcVowel Vl_i -> Nothing
    TcVowel Vl_I -> Nothing
    TcVowel v -> Just (wylieVowel v)
    TcFinal f -> Just (wylieFinal f)
    TcNumber n -> Just (wylieNumber n)
    TcPunctuation PMNyisTshegShad -> Nothing
    TcPunctuation PMRgyaGramShad -> Nothing
    TcPunctuation PMCaretDzudRtagsMeLong -> Nothing
    TcPunctuation p -> Just (wyliePunctuation p)
    TcSymbol s -> Just (wylieSymbol s)
    TcSpace m -> Just (wylieSpace m)
    _ -> Nothing

wylieConsonant :: Consonant -> Text
wylieConsonant = \case
    Ck -> "k"
    Ckh -> "kh"
    Cg -> "g"
    CgPLUSh -> "gh"
    Cng -> "ng"
    Cc -> "c"
    Cch -> "ch"
    Cj -> "j"
    Cny -> "ny"
    CT -> "T"
    CTh -> "Th"
    CD -> "D"
    CDPLUSh -> "Dh"
    CN -> "N"
    Ct -> "t"
    Cth -> "th"
    Cd -> "d"
    CdPLUSh -> "dh"
    Cn -> "n"
    Cp -> "p"
    Cph -> "ph"
    Cf -> "f"
    Cb -> "b"
    Cv -> "v"
    CbPLUSh -> "bh"
    Cm -> "m"
    Cts -> "ts"
    Ctsh -> "tsh"
    Cdz -> "dz"
    CdzPLUSh -> "dzh"
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
    CkPLUSSh -> "k+Sh"
    CR -> "R"
    Ckka -> ""
    CRra -> ""

wylieVowel :: Vowel -> Text
wylieVowel = \case
    VA -> "A"
    Vi -> "i"
    VI -> "I"
    Vu -> "u"
    VU -> "U"
    Vr_i -> ""
    Vr_I -> ""
    Vl_i -> ""
    Vl_I -> ""
    Ve -> "e"
    Vai -> "ai"
    Vo -> "o"
    Vau -> "au"
    V_i -> "-i"
    V_I -> "-I"

wylieFinal :: FinalMark -> Text
wylieFinal = \case
    FMAnusvara -> "M"
    FMVisarga -> "H"
    FMCandrabinduOrNasal -> "X"
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

wyliePunctuation :: PunctuationMark -> Text
wyliePunctuation = \case
    PMTsheg -> " "
    PMNonBreakingTsheg -> "*"
    PMShad -> "/"
    PMNyisShad -> "//"
    PMTshegShad -> ";"
    PMNyisTshegShad -> ""
    PMRinChenSpungsShad -> "|"
    PMRgyaGramShad -> ""
    PMCaretDzudRtagsMeLong -> ""
    PMGterTshigMgo -> ":"

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
    SMAsterisk -> "*"
    SMSlash -> "/"
    SMDoubleSlash -> "//"
    SMSemicolon -> ";"
    SMBar -> "|"
    SMColon -> ":"

wylieSpace :: SpaceMark -> Text
wylieSpace = \case
    SMSpace -> "_"

wylieConsonantAliases :: [(Text, Consonant)]
wylieConsonantAliases =
    [ ("g+h", CgPLUSh)
    , ("-t", CT)
    , ("-th", CTh)
    , ("-d", CD)
    , ("D+h", CDPLUSh)
    , ("-dh", CDPLUSh)
    , ("-d+h", CDPLUSh)
    , ("-n", CN)
    , ("d+h", CdPLUSh)
    , ("b+h", CbPLUSh)
    , ("dz+h", CdzPLUSh)
    , ("W", Cw)
    , ("Y", Cy)
    , ("-sh", CSh)
    ]

wylieVowelAliases :: [(Text, Vowel)]
wylieVowelAliases =
    [ ("O", Vo)
    ]

wylieFinalAliases :: [(Text, FinalMark)]
wylieFinalAliases =
    [ ("~M`", FMAnusvara)
    , ("~M", FMAnusvara)
    , ("~X", FMCandrabinduOrNasal)
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