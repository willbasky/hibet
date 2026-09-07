module Convert.Token where

import Data.Char (chr, isHexDigit, ord)
import Data.Text (Text)
import Numeric (readHex)
import Text.Printf (printf)
-- import Language (Consonant(CT))

-- | Превращает строку в формат "\x0f40\x0fad..."
toUnicodeEscape :: String -> String
toUnicodeEscape = concatMap charToHex
    where
        charToHex c = printf "\\x%04x" (ord c)

-- >>> toUnicodeEscape "ཨོུ"
-- "\\x0f68\\x0f74\\x0f7c"

fromUnicodeEscape :: String -> String
fromUnicodeEscape [] = []
fromUnicodeEscape ('\\' : 'x' : a : b : c : d : rest)
    | all isHexDigit [a, b, c, d] =
        let [(val, "")] = readHex [a, b, c, d]
         in chr val : fromUnicodeEscape rest
fromUnicodeEscape (x : xs) = x : fromUnicodeEscape xs

-- IR input source.
data TokenSource
    = TsUnicode
    | TsWylie
    deriving (Show, Eq)

-- High-level token classes shared by both tokenizers.
data TokenKind
    = TkConsonant
    | TkSubConsonant
    | TkVowel
    | TkFinal
    | TkNumber
    | TkHalfNumber
    | TkPunctuation
    | TkSpace
    | TkSymbol
    | TkUnknown
    deriving (Show, Eq)

-- Whether raw spelling should be preserved when alias forms normalize.
data AliasPolicy
    = PreserveRaw
    | Canonicalized
    deriving (Show, Eq)

data TokenIssueCode
    = UnknownChar
    | InvalidSequence
    | AmbiguousAlias
    | AutoNormalized
    deriving (Show, Eq)

data TokenIssue = TokenIssue
    { issueCode :: TokenIssueCode
    , issueMessage :: Text
    }
    deriving (Show, Eq)

-- Character offsets in the original input, half-open interval [start, end).
data Span = Span
    { offsetStart :: Int
    , offsetEnd :: Int
    }
    deriving (Show, Eq)

-- Canonical IR token used as contract between tokenizer and grammar layers.
data Token = Token
    { tokenRaw :: Text
    , tokenCanonical :: Text
    , tokenKind :: TokenKind
    , tokenSource :: TokenSource
    , tokenSpan :: Span
    , tokenAliasPolicy :: AliasPolicy
    , tokenIssues :: [TokenIssue]
    }
    deriving (Show, Eq)

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ fromUnicodeEscape "\\x0f74\\x0f7c"
-- "ོུ"

data Consonant
    = Ck -- ཀ \u0f40 
    | Ckh -- ཁ \u0f41 
    | Cg -- ག \u0f42 
    | CgPLUSh -- གྷ \u0f43 
    | Cng -- ང \u0f44 
    | Cc -- ཅ \u0f45 
    | Cch -- ཆ \u0f46 
    | Cj -- ཇ \u0f47 
    | Cny -- ཉ \u0f49 
    | CT -- ཊ \u0f4a 
    | CTh -- ཋ \u0f4b 
    | CD -- ཌ \u0f4c 
    | CDPLUSh -- ཌྷ \u0f4d 
    | CN -- ཎ \u0f4e 
    | Ct -- ཏ \u0f4f 
    | Cth -- ཐ \u0f50 
    | Cd -- ད \u0f51 
    | CdPLUSh -- དྷ \u0f52 
    | Cn -- ན \u0f53 
    | Cp -- པ \u0f54 
    | Cph -- ཕ \u0f55 
    | Cb -- བ \u0f56 
    | CbPLUSh -- བྷ \u0f57 
    | Cm -- མ \u0f58 
    | Cts -- ཙ \u0f59 
    | Ctsh -- ཚ \u0f5a 
    | Cdz -- ཛ \u0f5b 
    | CdzPLUSh -- ཛྷ \u0f5c 
    | Cw -- ཝ \u0f5d 
    | Czh -- ཞ \u0f5e 
    | Cz -- ཟ \u0f5f 
    | C' -- འ \u0f60 
    | Cy -- ཡ \u0f61 
    | Cr -- ར \u0f62 
    | Cl -- ལ \u0f63 
    | Csh -- ཤ \u0f64 
    | CSh -- ཥ \u0f65 
    | Cs -- ས \u0f66 
    | Ch -- ཧ \u0f67 
    | Ca -- ཨ \u0f68 
    | CkPLUSSh -- ཀྵ \u0f69 
    | CR -- ཪ \u0f6a 
    deriving (Show, Eq)

data Vowel
    = VA -- ཱ \u0f71
    | Vi   -- ི \u0f72 
    | VI   -- ཱི \u0f73
    | Vu   -- ུ \u0f74
    | VU     -- ཱུ \u0f75
    | Vr_i -- ྲྀ \u0f76
    | Vr_I   -- ཷ \u0f77
    | Vl_i -- ླྀ \u0f78
    | Vl_I   -- ཹ \u0f79
    | Ve  -- ེ \u0f7a
    | Vai -- ཻ \u0f7b
    | Vo -- ོ \u0f7c
    | Vau -- ཽ \u0f7d
    | V_i -- ྀ \u0f80 
    | V_I -- ཱྀ \u0f81

    -- | Vuo -- "ོུ"
    -- | Vui -- "ིུ" 
    -- | Vue -- "ེུ"
    deriving (Show, Eq)

data Number
    = N0 -- ༠
    | N1 -- ༡
    | N2 -- ༢
    | N3 -- ༣
    | N4 -- ༤
    | N5 -- ༥
    | N6 -- ༦
    | N7 -- ༧
    | N8 -- ༨
    | N9 -- ༩
    deriving (Show, Eq)

data HalfNumber
    = H_0 -- ༳
    | H_1 -- ༪
    | H_2 -- ༫
    | H_3 -- ༬
    | H_4 -- ༭
    | H_5 -- ༮
    | H_6 -- ༯
    | H_7 -- ༰
    | H_8 -- ༱
    | H_9 -- ༲
    deriving (Show, Eq)

-- >>> import Data.Char
-- >>> import qualified Data.Text as T
-- >>> T.map ((\c -> (c, ord c))) "ཀཁགངཅཆཇཉཏཐདནཔཕབམཙཚཛཝཞཟའཡརལཤསཧཨ"
-- Couldn't match expected type `Char' with actual type `(Char, Int)'
-- In the expression: (c_an4V, ord c_an4V)
-- In the first argument of `map', namely
--   `((\ c_an4V -> (c_an4V, ord c_an4V)))'
-- In the expression:
--   map
--     ((\ c_an4V -> (c_an4V, ord c_an4V)))
--     "ཀཁགངཅཆཇཉཏཐདནཔཕབམཙཚཛཝཞཟའཡརལཤསཧཨ"
