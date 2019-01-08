{-# LANGUAGE TemplateHaskell #-}

module Parse
       ( fromTibetan
       ) where

import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed
import Data.HashMap.Strict (HashMap)
-- import Data.Maybe (fromMaybe)
-- import Data.Text (Text)
-- import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HMS
-- import qualified Data.Text as T


syllables :: ByteString
syllables = $(embedFile "parser/tibetan-syllables")

fromTibetan :: ByteString -> Maybe ByteString
fromTibetan query =
    case checkTranslit query of
        Tibet -> HMS.lookup query $ makeTibetWylie
        Wylie -> Just query
        Other -> Nothing

-- t :: IO ()
-- t = do q <- BC.getLine
--        BC.putStrLn q
--        let q' = fromMaybe "0" (fromTibetan q)
--        BC.putStrLn q'

type WylieTibet = HashMap ByteString ByteString

type TibetWylie = HashMap ByteString ByteString

data Translit = Wylie | Tibet | Other
    deriving (Eq, Show)

makeWylieTibet :: WylieTibet
makeWylieTibet
    = HMS.fromList
    . map ((\(y,x) -> (y, BC.drop 1 x))
    . BC.span (<'|'))
    . BC.lines
    $ syllables

makeTibetWylie :: TibetWylie
makeTibetWylie
    = HMS.fromList
    . map ((\(y,x) -> (BC.drop 1 x, y))
    . BC.span (<'|'))
    . BC.lines
    $ syllables

checkTranslit :: ByteString -> Translit
checkTranslit query
    | HMS.member query $ makeWylieTibet = Wylie
    | HMS.member queryFirstWord $ makeTibetWylie = Tibet
    | otherwise = Other
  where
    queryFirstWord = BC.takeWhile (\x -> (x /= '\139') || (x /= ' ')) query
