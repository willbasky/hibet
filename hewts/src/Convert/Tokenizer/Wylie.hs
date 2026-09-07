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

-- wylie consonant => unicode
consonant :: HashMap Text Text
consonant = 
    HM.fromList
        [ ("k", "\x0f40")
        , ("kh", "\x0f41")
        , ("g", "\x0f42")
        , ("gh", "\x0f42\x0fb7")
        , ("g+h", "\x0f42\x0fb7")
        , ("ng", "\x0f44")
        , ("c", "\x0f45")
        , ("ch", "\x0f46")
        , ("j", "\x0f47")
        , ("ny", "\x0f49")
        , ("T", "\x0f4a")
        , ("-t", "\x0f4a")
        , ("Th", "\x0f4b")
        , ("-th", "\x0f4b")
        , ("D", "\x0f4c")
        , ("-d", "\x0f4c")
        , ("Dh", "\x0f4c\x0fb7")
        , ("D+h", "\x0f4c\x0fb7")
        , ("-dh", "\x0f4c\x0fb7")
        , ("-d+h", "\x0f4c\x0fb7")
        , ("N", "\x0f4e")
        , ("-n", "\x0f4e")
        , ("t", "\x0f4f")
        , ("th", "\x0f50")
        , ("d", "\x0f51")
        , ("dh", "\x0f51\x0fb7")
        , ("d+h", "\x0f51\x0fb7")
        , ("n", "\x0f53")
        , ("p", "\x0f54")
        , ("ph", "\x0f55")
        , ("b", "\x0f56")
        , ("bh", "\x0f56\x0fb7")
        , ("b+h", "\x0f56\x0fb7")
        , ("m", "\x0f58")
        , ("ts", "\x0f59")
        , ("tsh", "\x0f5a")
        , ("dz", "\x0f5b")
        , ("dzh", "\x0f5b\x0fb7")
        , ("dz+h", "\x0f5b\x0fb7")
        , ("w", "\x0f5d")
        , ("zh", "\x0f5e")
        , ("z", "\x0f5f")
        , ("'", "\x0f60")
        , ("y", "\x0f61")
        , ("r", "\x0f62")
        , ("l", "\x0f63")
        , ("sh", "\x0f64")
        , ("Sh", "\x0f65")
        , ("-sh", "\x0f65")
        , ("s", "\x0f66")
        , ("h", "\x0f67")
        , ("W", "\x0f5d")
        , ("Y", "\x0f61")
        , ("R", "\x0f6a")
        , ("f", "\x0f55\x0f39")
        , ("v", "\x0f56\x0f39")
        ]

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ consonant !? "R"
-- Just "ཪ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ consonant !? "gh"
-- Just "གྷ"

--    subjoined letters
subConsonant :: HashMap Text Text
subConsonant =
    HM.fromList
        [ ("k", "\x0f90")
        , ("kh", "\x0f91")
        , ("g", "\x0f92")
        , ("gh", "\x0f92\x0fb7")
        , ("g+h", "\x0f92\x0fb7")
        , ("ng", "\x0f94")
        , ("c", "\x0f95")
        , ("ch", "\x0f96")
        , ("j", "\x0f97")
        , ("ny", "\x0f99")
        , ("T", "\x0f9a")
        , ("-t", "\x0f9a")
        , ("Th", "\x0f9b")
        , ("-th", "\x0f9b")
        , ("D", "\x0f9c")
        , ("-d", "\x0f9c")
        , ("Dh", "\x0f9c\x0fb7")
        , ("D+h", "\x0f9c\x0fb7")
        , ("-dh", "\x0f9c\x0fb7")
        , ("-d+h", "\x0f9c\x0fb7")
        , ("N", "\x0f9e")
        , ("-n", "\x0f9e")
        , ("t", "\x0f9f")
        , ("th", "\x0fa0")
        , ("d", "\x0fa1")
        , ("dh", "\x0fa1\x0fb7")
        , ("d+h", "\x0fa1\x0fb7")
        , ("n", "\x0fa3")
        , ("p", "\x0fa4")
        , ("ph", "\x0fa5")
        , ("b", "\x0fa6")
        , ("bh", "\x0fa6\x0fb7")
        , ("b+h", "\x0fa6\x0fb7")
        , ("m", "\x0fa8")
        , ("ts", "\x0fa9")
        , ("tsh", "\x0faa")
        , ("dz", "\x0fab")
        , ("dzh", "\x0fab\x0fb7")
        , ("dz+h", "\x0fab\x0fb7")
        , ("w", "\x0fad")
        , ("zh", "\x0fae")
        , ("z", "\x0faf")
        , ("'", "\x0fb0")
        , ("y", "\x0fb1")
        , ("r", "\x0fb2")
        , ("l", "\x0fb3")
        , ("sh", "\x0fb4")
        , ("Sh", "\x0fb5")
        , ("-sh", "\x0fb5")
        , ("s", "\x0fb6")
        , ("h", "\x0fb7")
        , ("a", "\x0fb8")
        , ("W", "\x0fba")
        , ("Y", "\x0fbb")
        , ("R", "\x0fbc")
        ]

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ subConsonant !? "r"
-- Just "ྲ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ subConsonant !? "R"
-- Just "ྼ"

--  vowels
vowel :: HashMap Text Text
vowel =
    HM.fromList
        [ ("a", "\x0f68")
        , ("A", "\x0f71")
        , ("i", "\x0f72")
        , ("I", "\x0f71\x0f72")
        , ("u", "\x0f74")
        , ("U", "\x0f71\x0f74")
        , ("e", "\x0f7a")
        , ("E", "\x0f71\x0f7a")
        , ("ai", "\x0f7b")
        , ("o", "\x0f7c")
        , ("O", "\x0f71\x0f7c")
        , ("au", "\x0f7d")
        , ("-i", "\x0f80")
        , ("-I", "\x0f71\x0f80")
        ]

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ vowel !? "au"
-- Just "ཽ"

final :: HashMap Text Text
final =
    HM.fromList
        [ ("M", "\x0f7e")
        , ("~M`", "\x0f82")
        , ("~M", "\x0f83")
        , ("X", "\x0f37")
        , ("~X", "\x0f35")
        , ("H", "\x0f7f")
        , ("?", "\x0f84")
        , ("^", "\x0f39")
        , ("&", "\x0f85")
        ]

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ final !? "M"
-- Just "ཾ"

-- final symbols organized by class
finalClass :: HashMap Text Text
finalClass =
    HM.fromList
        [ ("M", "M")
        , ("~M`", "M")
        , ("~M", "M")
        , ("X", "X")
        , ("~X", "X")
        , ("H", "H")
        , ("?", "?")
        , ("^", "^")
        , ("&", "&")
        ]

-- other stand-alone symbols
standAlone :: HashMap Text Text
standAlone =
    HM.fromList
        [ ("0", "\x0f20")
        , ("1", "\x0f21")
        , ("2", "\x0f22")
        , ("3", "\x0f23")
        , ("4", "\x0f24")
        , ("5", "\x0f25")
        , ("6", "\x0f26")
        , ("7", "\x0f27")
        , ("8", "\x0f28")
        , ("9", "\x0f29")
        , (" ", "\x0f0b")
        , ("*", "\x0f0c")
        , ("/", "\x0f0d")
        , ("//", "\x0f0e")
        , (";", "\x0f0f")
        , ("|", "\x0f11")
        , ("!", "\x0f08")
        , (":", "\x0f14")
        , ("_", " ")
        , ("=", "\x0f34")
        , ("<", "\x0f3a")
        , (">", "\x0f3b")
        , ("(", "\x0f3c")
        , (")", "\x0f3d")
        , ("@", "\x0f04")
        , ("#", "\x0f05")
        , ("$", "\x0f06")
        , ("%", "\x0f07")
        ]

-- special characters: flag those if they occur out of context
special :: HashSet Text
special = HS.fromList [".", "+", "-", "~", "^", "?", "`", "]"]

-- superscripts: hashmap of superscript => set of letters or stacks below
superscript :: HashMap Text (HashSet Text)
superscript =
    HM.fromList
        [
            ( "r"
            , HS.fromList
                [ "k"
                , "g"
                , "ng"
                , "j"
                , "ny"
                , "t"
                , "d"
                , "n"
                , "b"
                , "m"
                , "ts"
                , "dz"
                , "k+y"
                , "g+y"
                , "m+y"
                , "b+w"
                , "ts+w"
                , "g+w"
                ]
            )
        ,
            ( "l"
            , HS.fromList
                [ "k"
                , "g"
                , "ng"
                , "c"
                , "j"
                , "t"
                , "d"
                , "p"
                , "b"
                , "h"
                ]
            )
        ,
            ( "s"
            , HS.fromList
                [ "k"
                , "g"
                , "ng"
                , "ny"
                , "t"
                , "d"
                , "n"
                , "p"
                , "b"
                , "m"
                , "ts"
                , "k+y"
                , "g+y"
                , "p+y"
                , "b+y"
                , "m+y"
                , "k+r"
                , "g+r"
                , "p+r"
                , "b+r"
                , "m+r"
                , "n+r"
                ]
            )
        ]

-- subscripts => set of letters above
subscript :: HashMap Text (HashSet Text)
subscript =
    HM.fromList
        [
            ( "y"
            , HS.fromList
                [ "k"
                , "kh"
                , "g"
                , "p"
                , "ph"
                , "b"
                , "m"
                , "r+k"
                , "r+g"
                , "r+m"
                , "s+k"
                , "s+g"
                , "s+p"
                , "s+b"
                , "s+m"
                ]
            )
        ,
            ( "r"
            , HS.fromList
                [ "k"
                , "kh"
                , "g"
                , "t"
                , "th"
                , "d"
                , "n"
                , "p"
                , "ph"
                , "b"
                , "m"
                , "sh"
                , "s"
                , "h"
                , "dz"
                , "s+k"
                , "s+g"
                , "s+p"
                , "s+b"
                , "s+m"
                , "s+n"
                ]
            )
        ,
            ( "l"
            , HS.fromList
                [ "k"
                , "g"
                , "b"
                , "r"
                , "s"
                , "z"
                ]
            )
        ,
            ( "w"
            , HS.fromList
                [ "k"
                , "kh"
                , "g"
                , "c"
                , "ny"
                , "t"
                , "d"
                , "ts"
                , "tsh"
                , "zh"
                , "z"
                , "r"
                , "l"
                , "sh"
                , "s"
                , "h"
                , "g+r"
                , "d+r"
                , "ph+y"
                , "r+g"
                , "r+ts"
                ]
            )
        ]

-- prefixes => set of consonants or stacks after
prefix :: HashMap Text (HashSet Text)
prefix =
    HM.fromList
        [
            ( "g"
            , HS.fromList
                [ "c"
                , "ny"
                , "t"
                , "d"
                , "n"
                , "ts"
                , "zh"
                , "z"
                , "y"
                , "sh"
                , "s"
                ]
            )
        ,
            ( "d"
            , HS.fromList
                [ "k"
                , "g"
                , "ng"
                , "p"
                , "b"
                , "m"
                , "k+y"
                , "g+y"
                , "p+y"
                , "b+y"
                , "m+y"
                , "k+r"
                , "g+r"
                , "p+r"
                , "b+r"
                ]
            )
        ,
            ( "b"
            , HS.fromList
                [ "k"
                , "g"
                , "c"
                , "t"
                , "d"
                , "ts"
                , "zh"
                , "z"
                , "sh"
                , "s"
                , "r"
                , "l"
                , "k+y"
                , "g+y"
                , "k+r"
                , "g+r"
                , "r+l"
                , "s+l"
                , "r+k"
                , "r+g"
                , "r+ng"
                , "r+j"
                , "r+ny"
                , "r+t"
                , "r+d"
                , "r+n"
                , "r+ts"
                , "r+dz"
                , "s+k"
                , "s+g"
                , "s+ng"
                , "s+ny"
                , "s+t"
                , "s+d"
                , "s+n"
                , "s+ts"
                , "r+k+y"
                , "r+g+y"
                , "s+k+y"
                , "s+g+y"
                , "s+k+r"
                , "s+g+r"
                , "l+d"
                , "l+t"
                , "k+l"
                , "s+r"
                , "z+l"
                , "s+w"
                ]
            )
        ,
            ( "m"
            , HS.fromList
                [ "kh"
                , "g"
                , "ng"
                , "ch"
                , "j"
                , "ny"
                , "th"
                , "d"
                , "n"
                , "tsh"
                , "dz"
                , "kh+y"
                , "g+y"
                , "kh+r"
                , "g+r"
                ]
            )
        ,
            ( "'"
            , HS.fromList
                [ "kh"
                , "g"
                , "ch"
                , "j"
                , "th"
                , "d"
                , "ph"
                , "b"
                , "tsh"
                , "dz"
                , "kh+y"
                , "g+y"
                , "ph+y"
                , "b+y"
                , "kh+r"
                , "g+r"
                , "d+r"
                , "ph+r"
                , "b+r"
                ]
            )
        ]

-- set of suffix letters
-- also included are some Skt letters b/c they occur often in suffix position in
-- Skt words
suffix :: HashSet Text
suffix =
    HS.fromList
        ["'", "g", "ng", "d", "n", "b", "m", "r", "l", "s", "N", "T", "-n", "-t"]

-- postfix => set of suffixes before
postfix :: HashMap Text (HashSet Text)
postfix =
    HM.fromList
        [ ("s", HS.fromList ["g", "ng", "b", "m"])
        , ("d", HS.fromList ["n", "r", "l"])
        ]

-- m_affixedsuff2
affixedPostfix :: HashSet Text
affixedPostfix = HS.fromList ["ng", "m"]

-- root letter index for very ambiguous three-stack syllables
ambiguous_root :: HashMap Text Word8
ambiguous_root =
    HM.fromList
        [ ("dgs", 1)
        , ("dms", 1)
        , ("dngs", 0)
        , ("'gs", 1)
        , ("'bs", 1)
        , ("mngs", 0)
        , ("mgs", 0)
        , ("bgs", 0)
        , ("dbs", 1)
        ]

ambiguous_wylie :: HashMap Text Text
ambiguous_wylie =
    HM.fromList
        [ ("dgs", "dgas")
        , ("dngs", "dangs")
        , ("dms", "dmas")
        , ("'gs", "'gas")
        , ("'bs", "'bas")
        , ("mngs", "mangs")
        , ("mgs", "mags")
        , ("bgs", "bags")
        , ("dbs", "dbas")
        ]

-- all these stacked consonant combinations don't need "+"s in them
stack :: HashSet Text
stack =
    HS.fromList
        [ "b+l"
        , "b+r"
        , "b+y"
        , "c+w"
        , "d+r"
        , "d+r+w"
        , "d+w"
        , "dz+r"
        , "g+l"
        , "g+r"
        , "g+r+w"
        , "g+w"
        , "g+y"
        , "h+r"
        , "h+w"
        , "k+l"
        , "k+r"
        , "k+w"
        , "k+y"
        , "kh+r"
        , "kh+w"
        , "kh+y"
        , "l+b"
        , "l+c"
        , "l+d"
        , "l+g"
        , "l+h"
        , "l+j"
        , "l+k"
        , "l+ng"
        , "l+p"
        , "l+t"
        , "l+w"
        , "m+r"
        , "m+y"
        , "n+r"
        , "ny+w"
        , "p+r"
        , "p+y"
        , "ph+r"
        , "ph+y"
        , "ph+y+w"
        , "r+b"
        , "r+d"
        , "r+dz"
        , "r+g"
        , "r+g+w"
        , "r+g+y"
        , "r+j"
        , "r+k"
        , "r+k+y"
        , "r+l"
        , "r+m"
        , "r+m+y"
        , "r+n"
        , "r+ng"
        , "r+ny"
        , "r+t"
        , "r+ts"
        , "r+ts+w"
        , "r+w"
        , "s+b"
        , "s+b+r"
        , "s+b+y"
        , "s+d"
        , "s+g"
        , "s+g+r"
        , "s+g+y"
        , "s+k"
        , "s+k+r"
        , "s+k+y"
        , "s+l"
        , "s+m"
        , "s+m+r"
        , "s+m+y"
        , "s+n"
        , "s+n+r"
        , "s+ng"
        , "s+ny"
        , "s+p"
        , "s+p+r"
        , "s+p+y"
        , "s+r"
        , "s+t"
        , "s+ts"
        , "s+w"
        , "sh+r"
        , "sh+w"
        , "t+r"
        , "t+w"
        , "th+r"
        , "ts+w"
        , "tsh+w"
        , "z+l"
        , "z+w"
        , "zh+w"
        ]

-- a map used to split the input string into tokens for toUnicode().
-- all letters which start tokens longer than one letter are mapped to the max
-- length of tokens starting with that letter.

tokensStart :: HashMap Char Word8
tokensStart =
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
tokens :: HashSet Text
tokens =
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
        maxLen <- fromIntegral <$> (tokensStart !? c)
        longestFrom maxLen

    longestFrom n
        | n < 2 = Nothing
        | T.length source < n = longestFrom (n - 1)
        | otherwise =
            let candidate = T.take n source
             in if HS.member candidate tokens
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