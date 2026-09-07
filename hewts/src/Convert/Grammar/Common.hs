module Convert.Grammar.Common where

import Data.Char
import Data.Either.Extra (fromEither, mapLeft)
import Data.Foldable (foldl')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Massiv.Vector (Comp (..), P, Vector)
import qualified Data.Massiv.Vector as V
import Data.Maybe (fromMaybe)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseEither :: Parser a -> Text -> Either Text a
parseEither p t = mapLeft (T.pack . errorBundlePretty) $ runParser p "" t

recovering :: Parser a -> Parser a
recovering p = withRecovery (\e -> registerParseError e *> skipGarbage *> p) p

skipGarbage :: Parser ()
skipGarbage = skipMany (satisfy (not . flip HS.member punctuation)) <* optional pPunctuation


consonants :: Vector P Char
consonants =
    V.fromList
        Seq
        [ 'ཀ' -- 1 
        , 'ཁ' -- 2 
        , 'ག' -- 3 
        , 'ང' -- 4 
        , 'ཅ' -- 5 
        , 'ཆ' -- 6 
        , 'ཇ' -- 7 
        , 'ཉ' -- 8 
        , 'ཏ' -- 9 
        , 'ཐ' -- 10
        , 'ད' -- 11
        , 'ན' -- 12
        , 'པ' -- 13
        , 'ཕ' -- 14
        , 'བ' -- 15
        , 'མ' -- 16
        , 'ཙ' -- 17
        , 'ཚ' -- 18
        , 'ཛ' -- 19
        , 'ཝ' -- 20
        , 'ཞ' -- 21
        , 'ཟ' -- 22
        , 'འ' -- 23
        , 'ཡ' -- 24
        , 'ར' -- 25
        , 'ལ' -- 26
        , 'ཤ' -- 27
        , 'ས' -- 28
        , 'ཧ' -- 29
        , 'ཨ' -- 30
        , 'ཊ' -- 31
        , 'ཋ' -- 32
        , 'ཌ' -- 33
        , 'ཎ' -- 34
        , 'ཥ' -- 35
        ]

-- Root consonants under superfix
subConsonants :: Vector P Char
subConsonants =
    V.fromList
        Seq
        [ chr 0x0F90 -- 'ཀ' -- 1 
        , chr 0x0F91 -- 'ཁ' -- 2 
        , chr 0x0F92 -- 'ག' -- 3 
        , chr 0x0F94 -- 'ང' -- 4 
        , chr 0x0F95 -- 'ཅ' -- 5 
        , chr 0x0F96 -- 'ཆ' -- 6 
        , chr 0x0F97 -- 'ཇ' -- 7 
        , chr 0x0F99 -- 'ཉ' -- 8 
        , chr 0x0F9F -- 'ཏ' -- 9 
        , chr 0x0FA0 -- 'ཐ' -- 10
        , chr 0x0FA1 -- 'ད' -- 11
        , chr 0x0FA3 -- 'ན' -- 12
        , chr 0x0FA4 -- 'པ' -- 13
        , chr 0x0FA5 -- 'ཕ' -- 14
        , chr 0x0FA6 -- 'བ' -- 15
        , chr 0x0FA8 -- 'མ' -- 16
        , chr 0x0FA9 -- 'ཙ' -- 17
        , chr 0x0FAA -- 'ཚ' -- 18
        , chr 0x0FAB -- 'ཛ' -- 19
        , chr 0x0FBA -- 'ཝ' -- 20
        , chr 0x0FAE -- 'ཞ' -- 21
        , chr 0x0FAF -- 'ཟ' -- 22
        , chr 0x0FB0 -- 'འ' -- 23
        , chr 0x0FBB -- 'ཡ' -- 24 
        , chr 0x0FBC -- 'ར' -- 25 
        , chr 0x0FB3 -- 'ལ' -- 26
        , chr 0x0FB4 -- 'ཤ' -- 27
        , chr 0x0FB6 -- 'ས' -- 28
        , chr 0x0FB7 -- 'ཧ' -- 29
        , chr 0x0FB8 -- 'ཨ' -- 30
        , chr 0x0F9A -- 'ཊ' -- 31
        , chr 0x0F9B -- 'ཋ' -- 32
        , chr 0x0F9C -- 'ཌ' -- 33
        , chr 0x0F9E -- 'ཎ' -- 34
        , chr 0x0FB5 -- 'ཥ' -- 35
        ]

selectConsonants :: Vector P Char -> [Int] -> Text
selectConsonants vec = foldl' helper T.empty
    where
        helper acc i = case V.index vec $ i - 1 of
            Nothing -> acc
            Just c -> acc :> c

formatConstants :: Text -> Text
formatConstants txt = "[ '" `T.append` T.replace "," "', '" comma `T.append` "' ]"
    where
        comma = T.intersperse ',' txt

fetchChar :: Vector P Char -> Int -> Char
fetchChar charVec i = charVec V.! i - 1

fetchChars :: Vector P Char -> [Int] -> HashSet Char
fetchChars charVec = HS.fromList . foldl' helper []
    where
        helper acc i = case V.index charVec $ i - 1 of
            Nothing -> acc
            Just c -> c : acc

-- Root characters (30 consonants)
tibetanConsonant :: HashSet Char
tibetanConsonant = fetchChars consonants [1..30]

pRootConsonant :: Parser Char
pRootConsonant = satisfy (`HS.member` tibetanConsonant) <?> "One of 30 Tibetan consonants"

--
-- Sanskrit characters (5: ཊ, ཋ, ཌ, ཎ, ཥ)
sanskritConsonant :: HashSet Char
sanskritConsonant = fetchChars consonants [31..35]

pSanskrit :: Parser Char
pSanskrit = satisfy (`HS.member` sanskritConsonant) <?> "One of the Sanskrit consonants [ཊ, ཋ, ཌ, ཎ, ཥ]"

--
-- Vowels (4: ི, ེ , ོ , ུ , plus achung for inherent 'a')
vowel :: HashSet Char
vowel = HS.fromList [chr 0x0F72, chr 0x0F7A, chr 0x0F74, chr 0x0F7C] 

vowelLongA :: Parser Char
vowelLongA = char 'ཱ' <?> "Long vowel འ"

pVowel :: Parser Char
pVowel = satisfy (`HS.member` vowel) <?> "Vowel character"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pVowel "ུ"
-- Right 'ུ'
--
-- Prefix characters (5: ['ག', 'ད', 'བ', 'མ', 'འ'])

-- Prefix 'ག' - 3
pPrefixGa :: Parser Char 
pPrefixGa = char (fetchChar consonants 3) <?> "Prefix ག"

-- Prefix 'ད' - 11
pPrefixDa :: Parser Char 
pPrefixDa = char (fetchChar consonants 11) <?> "Prefix ད"

-- Prefix 'བ' - 15
pPrefixBa :: Parser Char 
pPrefixBa = char (fetchChar consonants 15) <?> "Prefix བ"

-- Prefix 'མ' - 16
pPrefixMa :: Parser Char 
pPrefixMa = char (fetchChar consonants 16) <?> "Prefix མ"

-- Prefix 'འ' - 23
pPrefixA :: Parser Char 
pPrefixA = char (fetchChar consonants 23) <?> "Prefix འ"

--
-- Superfix

-- Superfix characters (3: ['ར', 'ལ', 'ས'])
-- Superfix 'ར' - 25
pSuperfixRa :: Parser Char 
pSuperfixRa = char (fetchChar consonants 25) <?> "Superfix ར"

-- Superfix 'ལ' - 26
pSuperfixLa :: Parser Char 
pSuperfixLa = char (fetchChar consonants 26) <?> "Superfix ལ"

-- Superfix 'ས' - 28
pSuperfixSa :: Parser Char 
pSuperfixSa = char (fetchChar consonants 28) <?> "Superfix ས"

--
-- Subfix

-- Subfix characters (4: ཝ, ཡ, ར, ལ)
-- Subfix 'ཝ' - 20
pSubfixWa :: Parser Char
pSubfixWa = char (chr 0x0FAD) <?> "Subfix ཝ"

-- Subfix 'ཡ' - 24
pSubfixYa :: Parser Char
pSubfixYa = char (chr 0x0FB1) <?> "Subfix ཡ"

-- Subfix 'ར' - 25
pSubfixRa :: Parser Char
pSubfixRa = char (chr 0x0FB2) <?> "Subfix ར" 

-- Subfix 'ལ' - 26
pSubfixLa :: Parser Char
pSubfixLa = char (chr 0x0FB3) <?> "Subfix ལ"

--
-- Suffix characters (10: ['འ', 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'ར', 'ལ', 'ས'])
suffix :: HashSet Char
suffix = fetchChars consonants [ 3, 4, 11, 12, 15, 16, 23, 25, 26, 28]

pSuffix :: Parser Char
pSuffix = satisfy (`HS.member` suffix) <?> "Suffix character"

--
-- Postfix characters (2: ['ད', 'ས'])
postfix :: HashSet Char
postfix = fetchChars consonants [11, 28]

pPostfix :: Parser Char
pPostfix = satisfy (`HS.member` postfix) <?> "Postfix character"

-- Subfix 'ད' - 11
pPostfixDa :: Parser Char
pPostfixDa = char (fetchChar consonants 11) <?> "Postfix ད"

-- Subfix 'ས' - 28
pPostfixSa :: Parser Char
pPostfixSa = char (fetchChar consonants 28) <?> "Postfix ས"

--
-- Punctuation 
punctuation :: HashSet Char
punctuation =
    HS.fromList
        [ chr 0x0F0B -- '་' -- tsheg 
        , chr 0x0F0C -- tsheg bstar -- non-breaking tsheg
        , chr 0x0F0D -- '།' -- shad
        , chr 0x0F0E -- '༎' -- nyis shad
        , chr 0x0F0F -- '༏' -- tsheg shad
        , chr 0x0F10 -- '༐' -- nyis tsheg shad
        , chr 0x0F11 -- '༑' -- rin chen spungs shad
        , chr 0x0F14 -- '༔' -- gter tsheg
        , ' ' -- space
        ]

pPunctuation :: Parser Char
pPunctuation = satisfy (`HS.member` punctuation) <?> "Punctuation character"

numbers :: HashSet Char 
numbers = HS.fromList
        [ '༠'
        , '༡'
        , '༢'
        , '༣'
        , '༤'
        , '༥'
        , '༦'
        , '༧'
        , '༨'
        , '༩'
        ]

pNumber :: Parser Text
pNumber = T.singleton <$> satisfy (`HS.member` numbers) <?> "Number character"
