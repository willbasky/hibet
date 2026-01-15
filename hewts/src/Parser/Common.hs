module Parser.Common where

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
        [ chr 0x0F90 --'ཀ' -- 1 
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

getConsonants :: [Int] -> Text
getConsonants = foldl' helper T.empty
    where
        helper acc i = case V.index consonants $ i - 1 of
            Nothing -> acc
            Just c -> acc :> c

formatConstants :: Text -> Text
formatConstants txt = "[ '" `T.append` T.replace "," "', '" comma `T.append` "' ]"
    where
        comma = T.intersperse ',' txt

fetchChar :: Vector P Char -> Int -> Char
fetchChar consonants i = consonants V.! i - 1

fetchChars :: Vector P Char -> [Int] -> HashSet Char
fetchChars consonants = HS.fromList . foldl' helper []
    where
        helper acc i = case V.index consonants $ i - 1 of
            Nothing -> acc
            Just c -> c : acc

-- Root characters (30 consonants)
tibetanConsonant :: HashSet Char
tibetanConsonant = fetchChars consonants [1..30]

pRootConsonant :: Parser Char
pRootConsonant = satisfy (`HS.member` tibetanConsonant) <?> "<<root character>>"

--
-- Sanskrit characters (5: ཊ, ཋ, ཌ, ཎ, ཥ)
sanskritConsonant :: HashSet Char
sanskritConsonant = fetchChars consonants [31..35]

pSanskrit :: Parser Char
pSanskrit = satisfy (`HS.member` sanskritConsonant) <?> "<<sanskrit character>>"

--
-- Prefix characters (5: ['ག', 'ད', 'བ', 'མ', 'འ'])
prefix :: HashSet Char
prefix = fetchChars consonants [3, 11, 15, 16, 23]

pPrefix :: Parser Char
pPrefix = satisfy (`HS.member` prefix) <?> "<<prefix character>>"

--
-- Vowels (4: ི, ེ , ོ , ུ , plus achung for inherent 'a')
vowel :: HashSet Char
vowel = HS.fromList [chr 0x0F72, chr 0x0F7A, chr 0x0F74, chr 0x0F7C]

vowelLongA :: Parser Char
vowelLongA = char 'ཱ'

pVowel :: Parser Char
pVowel = satisfy (`HS.member` vowel) <?> "<<explicit vowel character>>"

--
-- Suffix characters (10: ['འ', 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'ར', 'ལ', 'ས'])
suffix :: HashSet Char
suffix = fetchChars consonants [ 3, 4, 11, 12, 15, 16, 23, 25, 26, 28]

pSuffix :: Parser Char
pSuffix = satisfy (`HS.member` suffix) <?> "<<suffix character>>"

--
-- Postfix characters (2: ['ད', 'ས'])
postfix :: HashSet Char
postfix = fetchChars consonants [11, 28]

pPostfix :: Parser Char
pPostfix = satisfy (`HS.member` postfix) <?> "<<postfix character>>"

--
-- Superfix

-- Superfix characters (3: ['ར', 'ལ', 'ས'])
superfix :: HashSet Char
superfix = fetchChars consonants [25, 26, 28]

pSuperfix :: Parser Char
pSuperfix = satisfy (`HS.member` superfix) <?> "<<superfix character>>"

-- Superfix 'ར' - 25
pSuperfixRa :: Parser Char 
pSuperfixRa = char $ fetchChar consonants 25

-- Superfix 'ལ' - 26
pSuperfixLa :: Parser Char 
pSuperfixLa = char $ fetchChar consonants 26

-- Superfix 'ས' - 28
pSuperfixSa :: Parser Char 
pSuperfixSa = char $ fetchChar consonants 28

--
-- Subfix

-- Subfix characters (4: ཝ, ཡ, ར, ལ)
subfix :: HashSet Char
subfix = fetchChars consonants [20, 24, 25, 26] 

pSubfix :: Parser Char
pSubfix = satisfy (`HS.member` subfix) <?> "<<subfix character>>"

-- Subfix 'ཝ' - 20
pSubfixWa :: Parser Char
pSubfixWa = char $ chr 0x0FAD

-- Subfix 'ཡ' - 24
pSubfixYa :: Parser Char
pSubfixYa = char $ chr 0x0FB1

-- Subfix 'ར' - 25
pSubfixRa :: Parser Char
pSubfixRa = char $ chr 0x0FB2

-- Subfix 'ལ' - 26
pSubfixLa :: Parser Char
pSubfixLa = char $ chr 0x0FB3