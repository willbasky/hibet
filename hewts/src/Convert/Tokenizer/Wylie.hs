{- HLINT ignore "Use camelCase" -}

module Convert.Tokenizer.Wylie where

import Convert.Token
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Control.Applicative (asum)
import Data.Maybe (fromMaybe)


-- special characters: flag those if they occur out of context
special :: HashSet Text
special = HS.fromList [".", "+", "-", "~", "^", "?", "`", "]"]

-- a map used to split the input string into tokens for toUnicode().
-- all letters which start tokens longer than one letter are mapped to the max
-- length of tokens starting with that letter.
tokenStart :: HashMap Char Word8
tokenStart =
    HM.fromList
        [ ('S', 2)
        , ('/', 2)
        , ('d', 4)
        , ('g', 3)
        , ('b', 3)
        , ('D', 3)
        , ('z', 2)
        , ('~', 3)
        , ('-', 4)
        , ('T', 2)
        , ('a', 2)
        , ('k', 2)
        , ('t', 3)
        , ('s', 2)
        , ('c', 2)
        , ('n', 2)
        , ('p', 2)
        , ('\r', 2)
        ]

-- also for tokenization - a set of tokens longer than one letter
longToken :: HashSet Text
longToken =
    HS.fromList
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
nextChunk source =
    case T.uncons source of
        Nothing -> (T.empty, T.empty)
        Just (c, _) ->
            case longestComposite c of
                Just tok -> (tok, T.drop (T.length tok) source)
                Nothing -> (T.take 1 source, T.drop 1 source)
  where
    longestComposite c = do
        maxLen <- fromIntegral <$> (tokenStart !? c)
        longestFrom maxLen

    longestFrom n
        | n < 2 = Nothing
        | T.length source < n = longestFrom (n - 1)
        | otherwise =
            let candidate = T.take n source
             in if HS.member candidate longToken
                    then Just candidate
                    else longestFrom (n - 1)

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