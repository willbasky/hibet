-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
       ( Parser
       , fromTibetan
       , toTibet
       ) where

-- import Data.ByteString.Char8 (ByteString)
-- import Data.FileEmbed
import Data.HashMap.Strict (HashMap)
import Data.Char (isLatin1)
-- import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, try)
-- import Text.Megaparsec.Char.Lexer (lexeme)
-- import Text.Megaparsec.Char (oneOf)

-- import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
-- import qualified Data.Set             as E
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Text.IO as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
-- import qualified Text.Megaparsec.Char.Lexer as MCL


-- The parser
type Parser = Parsec Void Text

fromTibetan :: Text -> Text -> Maybe Text
fromTibetan query syllables =
    case checkTranslit query of
        Tibet -> toWylie query syllables
        Wylie -> Just query
        Other -> Nothing

toWylie :: Text -> Text -> Maybe Text
toWylie query syllables = fmap T.unwords
    $ mapM (\x -> HMS.lookup x $ makeTibetWylie syllables)
    $ filter (/= "")
    $ T.splitOn "\3851" query

toTibet :: Text -> Text -> Text
toTibet translation syllables = T.unwords
    $ mapMaybe (\x -> if isJust (parseWylie x) then convertorT (parseWylie x) else Just x)
    $ filter (/= "")
    $ T.splitOn " " translation
  where
    parseWylie :: Text -> Maybe String
    parseWylie = M.parseMaybe (identifier2 syllables)
    convertorT :: Maybe String -> Maybe Text
    convertorT = join . mapM (\t -> if isJust (lookerW t) then lookerW t else Just t) . fmap T.pack
    lookerW :: Text -> Maybe Text
    lookerW q = HMS.lookup q $ makeWylieTibet syllables


-- parseTibet2 :: Text -> Text -> Maybe Text
-- parseTibet2 syllables query = fmap T.unwords
--     $ mapM (\x -> HMS.lookup x $ makeTibetWylie syllables)
--     $ filter (/= "")
--     $ T.splitOn "\3851" query

-- tibetanText :: Parser Text
-- tibetanText = AT.takeTill (\x -> AT.isEndOfLine x || (x == '\3851') || (x == '\r'))

-- parsed :: Text -> Maybe [Text]
-- parsed = AT.maybeResult . AT.parse (tibetanText `AT.sepBy` tibetanSpace)

-- parsed2 :: Text -> Text -> Maybe Text
-- parsed2 syllables = fmap T.concat . AT.maybeResult . AT.parse (parserT syllables)

syllablesListWylie :: Text -> [String]
syllablesListWylie syllables = map T.unpack $ HMS.keys (makeWylieTibet syllables)

identifier2 :: Text -> Parser String
identifier2 syllables = try (p >>= check)
  where
    p :: Parser String
    p = (:) <$> MC.letterChar <*> many M.anySingle
    check :: String -> Parser String
    check x = if x `elem` syllablesListWylie syllables
                then return x
                else fail $ "keyword " ++ show x ++ " cannot be an identifier"

-- lexeme :: Parser a -> Parser a
-- lexeme = MCL.lexeme sc

-- sc :: Parser ()
-- sc = MC.space1

-- rws :: [String] -- list of reserved words
-- rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

-- identifier :: Parser String
-- identifier = (lexeme . try) (p >>= check)
--   where
--     p       = (:) <$> MC.letterChar <*> many MC.alphaNumChar
--     check x = if x `elem` rws
--                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
--                 else return x
--  HMS.lookup query $ makeTibetWylie syllables

-- t :: IO ()
-- t = do q <- BC.getLine
--        BC.putStrLn q
--        let q' = fromMaybe "0" (fromTibetan q)
--        BC.putStrLn q'

type WylieTibet = HashMap Text Text

type TibetWylie = HashMap Text Text

data Translit = Wylie | Tibet | Other
    deriving (Eq, Show)

makeWylieTibet :: Text -> WylieTibet
makeWylieTibet
    = HMS.fromList
    . map ((\(y,x) -> (y, T.drop 1 x))
    . T.span (<'|'))
    . T.lines

makeTibetWylie :: Text -> TibetWylie
makeTibetWylie
    = HMS.fromList
    . map ((\(y,x) -> (T.drop 1 x, y))
    . T.span (<'|'))
    . T.lines

checkTranslit :: Text -> Translit
checkTranslit query =
    if queryFirstWord then Wylie else Tibet
  where
    queryFirstWord = T.all isLatin1 query
