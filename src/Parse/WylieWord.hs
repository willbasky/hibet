module Parse.WylieWord
  (
    wylieWord
  ) where


import Parse.Type ( Parser )

import Control.Applicative (Alternative (some), (<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import Data.Char ( isAsciiLower, isAsciiUpper )


---------------------------------------------------------------------
-- Parse wylie script
---------------------------------------------------------------------

wylieSpecial :: Parser Char
wylieSpecial =
        MC.char '+'
    <|> MC.char '\''
    <|> MC.char ':'
    <|> MC.char '-'
    <|> MC.char '.'
    <|> MC.char '%'
    <|> MC.char '@'
    <|> MC.char '~'

asciiLetters :: Parser Char
asciiLetters = M.satisfy (\x -> isAsciiUpper x || isAsciiLower x)

-- Parse wylie char
wylieChar  :: Parser Char
wylieChar = M.try $ asciiLetters <|> wylieSpecial

-- Parse several wylie chars
wylieSeq :: Parser Char -> Parser Text
wylieSeq = fmap T.pack . some

-- | Parse wylie syllable
wylieSyl :: Parser Text
wylieSyl = do
  MC.space
  s <- wylieSeq wylieChar
  MC.space
  pure s

wylieWord :: Parser [Text]
wylieWord = M.try $ some wylieSyl
