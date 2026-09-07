{- HLINT ignore "Use camelCase" -}

module Convert.Tokenizer.Wylie where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Convert.Token
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Trie as Trie


-- special characters: flag those if they occur out of context
special :: HashSet Text
special = HS.fromList [".", "+", "-", "~", "^", "?", "`", "]"]

-- Longest-match lookup over known multi-char Wylie tokens.
longTokenTrie :: Trie.Trie ()
longTokenTrie =
    Trie.fromList [(TE.encodeUtf8 tok, ()) | tok <- longTokenList]

longTokenList :: [Text]
longTokenList =
    [ "k+Sh"
    , "b+l"
    , "-d+h"
    , "dz+h"
    , "-dh"
    , "-sh"
    , "-th"
    , "D+h"
    , "b+h"
    , "d+h"
    , "dzh"
    , "g+h"
    , "tsh"
    , "~M`"
    , "-I"
    , "-d"
    , "-i"
    , "-n"
    , "-t"
    , "//"
    , "Dh"
    , "Sh"
    , "Th"
    , "ai"
    , "au"
    , "bh"
    , "ch"
    , "dh"
    , "dz"
    , "gh"
    , "kh"
    , "ng"
    , "ny"
    , "ph"
    , "sh"
    , "th"
    , "ts"
    , "zh"
    , "~M"
    , "~X"
    , "\r\n"
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
    | otherwise =
        case Trie.match longTokenTrie sourceBytes of
            Just (prefix, _, rest)
                | not (BS.null prefix) ->
                    (TE.decodeUtf8 prefix, TE.decodeUtf8 rest)
            _ -> (T.take 1 source, T.drop 1 source)
  where
    sourceBytes :: ByteString
    sourceBytes = TE.encodeUtf8 source

classifyToken :: Span -> Text -> Token
classifyToken span raw =
    fromMaybe (mkUnknown TsWylie span raw) $ asum
        [ lookupAs mkConsonant consonantTokenMap
        , lookupAs mkVowel vowelTokenMap
        , lookupAs mkFinal finalTokenMap
        , lookupAs mkNumber numberTokenMap
        , lookupAs mkPunctuation punctuationTokenMap
        , lookupAs mkSymbol symbolTokenMap
        , if raw == "_"
            then Just (mkSpace TsWylie span raw SMSpace)
            else Nothing
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
    lookupAs constructor tokenMap =
        constructor TsWylie span raw <$> HM.lookup raw tokenMap


consonantTokenMap :: HashMap Text Consonant
consonantTokenMap =
    HM.fromList
        [ ("k", Ck)
        , ("kh", Ckh)
        , ("g", Cg)
        , ("gh", CgPLUSh)
        , ("g+h", CgPLUSh)
        , ("ng", Cng)
        , ("c", Cc)
        , ("ch", Cch)
        , ("j", Cj)
        , ("ny", Cny)
        , ("T", CT)
        , ("-t", CT)
        , ("Th", CTh)
        , ("-th", CTh)
        , ("D", CD)
        , ("-d", CD)
        , ("Dh", CDPLUSh)
        , ("D+h", CDPLUSh)
        , ("-dh", CDPLUSh)
        , ("-d+h", CDPLUSh)
        , ("N", CN)
        , ("-n", CN)
        , ("t", Ct)
        , ("th", Cth)
        , ("d", Cd)
        , ("dh", CdPLUSh)
        , ("d+h", CdPLUSh)
        , ("n", Cn)
        , ("p", Cp)
        , ("ph", Cph)
        , ("b", Cb)
        , ("bh", CbPLUSh)
        , ("b+h", CbPLUSh)
        , ("m", Cm)
        , ("ts", Cts)
        , ("tsh", Ctsh)
        , ("dz", Cdz)
        , ("dzh", CdzPLUSh)
        , ("dz+h", CdzPLUSh)
        , ("w", Cw)
        , ("W", Cw)
        , ("zh", Czh)
        , ("z", Cz)
        , ("'", C')
        , ("y", Cy)
        , ("Y", Cy)
        , ("r", Cr)
        , ("l", Cl)
        , ("sh", Csh)
        , ("Sh", CSh)
        , ("-sh", CSh)
        , ("s", Cs)
        , ("h", Ch)
        , ("a", Ca)
        , ("k+Sh", CkPLUSSh)
        , ("R", CR)
        ]

vowelTokenMap :: HashMap Text Vowel
vowelTokenMap =
    HM.fromList
        [ ("A", VA)
        , ("i", Vi)
        , ("I", VI)
        , ("u", Vu)
        , ("U", VU)
        , ("e", Ve)
        , ("ai", Vai)
        , ("o", Vo)
        , ("O", Vo)
        , ("au", Vau)
        , ("-i", V_i)
        , ("-I", V_I)
        ]

finalTokenMap :: HashMap Text FinalMark
finalTokenMap =
    HM.fromList
        [ ("M", FMAnusvara)
        , ("~M`", FMAnusvara)
        , ("~M", FMAnusvara)
        , ("X", FMCandrabinduOrNasal)
        , ("~X", FMCandrabinduOrNasal)
        , ("H", FMVisarga)
        , ("?", FMHalanta)
        , ("^", FMCaret)
        , ("&", FMYigMgo)
        ]

numberTokenMap :: HashMap Text Number
numberTokenMap =
    HM.fromList
        [ ("0", N0)
        , ("1", N1)
        , ("2", N2)
        , ("3", N3)
        , ("4", N4)
        , ("5", N5)
        , ("6", N6)
        , ("7", N7)
        , ("8", N8)
        , ("9", N9)
        ]

punctuationTokenMap :: HashMap Text PunctuationMark
punctuationTokenMap =
    HM.fromList
        [ (" ", PMTsheg)
        , ("*", PMNonBreakingTsheg)
        , ("/", PMShad)
        , ("//", PMNyisShad)
        , (";", PMTshegShad)
        , ("|", PMRinChenSpungsShad)
        , (":", PMGterTshigMgo)
        ]

symbolTokenMap :: HashMap Text SymbolMark
symbolTokenMap =
    HM.fromList
        [ ("!", SMExclamation)
        , ("@", SMAt)
        , ("#", SMHash)
        , ("$", SMDollar)
        , ("%", SMPercent)
        , ("=", SMEqual)
        , ("<", SMLt)
        , (">", SMGt)
        , ("(", SMLParen)
        , (")", SMRParen)
        ]