module Parser where

import Text.Megaparsec
import Data.Text (Text)
import Parser.Common
import qualified Parser.Structure as S


allRules :: Parser Text
allRules = choice [S.structure1, S.structure2]

