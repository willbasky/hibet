module Parser where

import Data.Text (Text)
import Parser.Common
import qualified Parser.Structure as S
import Text.Megaparsec

allRules :: Parser Text
allRules =
    choice
        [ S.structure1
        , S.structure2
        , S.structure3
        , S.structure4
        , S.structure5
        , S.structure6
        , S.structure7
        , S.structure8
        ]
