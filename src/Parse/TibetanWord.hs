module Parse.TibetanWord
  (
    tibetanWord
  ) where


import Parse.Type (Parser)

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Char (isMark)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML


---------------------------------------------------------------------
-- Parse wylie script
---------------------------------------------------------------------

symbolM :: M.Tokens Text -> Parser (M.Tokens Text)
symbolM = ML.symbol MC.space

tibetanDotM :: Parser Text
tibetanDotM = symbolM "་"

tibetanEnd :: Parser Text
tibetanEnd = symbolM "།"

tibetanEnds :: Parser Text
tibetanEnds = M.try $ tibetanDotM <|> tibetanEnd

tibetanMarks :: Parser Char
tibetanMarks = M.satisfy isMark

-- Parse several wylie chars
someChar :: Parser Char -> Parser Text
someChar = fmap T.pack . some

tibetanString :: Parser Text
tibetanString =
  someChar $ M.try $ MC.alphaNumChar <|> tibetanMarks

tibetanSyl :: Parser Text
tibetanSyl = do
  MC.space
  s <- tibetanString
  _ <- many tibetanEnds
  MC.space
  pure s

tibetanWord :: Parser [Text]
tibetanWord = M.try $ some tibetanSyl
