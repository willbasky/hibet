{-
Tibetan spelling grammar 4.10 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint10 (pConstraint10) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Token
  ( SubConsonant (..)
  , Token
  , TokenCanonical (TcSubConsonant)
  , tokenCanonical
  )
import Data.Maybe (maybeToList)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint10 :: Parser [Token]
pConstraint10 =
  MP.choice
    [ MP.try $ parseConstraint10 GP.pSuperfixRa pRoots1 GP.pSubfixYa
    , MP.try $ parseConstraint10 GP.pSuperfixSa pRoots2 (GP.pSubfixYa <|> GP.pSubfixRa)
    , MP.try $ parseConstraint10 GP.pSuperfixSa pRoot3 GP.pSubfixRa
    , MP.try $ parseConstraint10 GP.pSuperfixRa pRoot4 GP.pSubfixWa
    ]

parseConstraint10 :: Parser Token -> Parser Token -> Parser Token -> Parser [Token]
parseConstraint10 parseSuperfix parseRoot parseSubfix = do
  superfix <- parseSuperfix
  root <- parseRoot
  subfix <- parseSubfix
  vowel <- MP.optional GP.pVowel
  pure $ [superfix, root, subfix] <> maybeToList vowel

-- (1) root group [ 'ཀ', 'ག', 'མ' ] under superfix ར and above subfix ཡ
pRoots1 :: Parser Token
pRoots1 = pAllowedRoot [SCk, SCg, SCm]

-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] under superfix ས and above subfix ཡ or ར
pRoots2 :: Parser Token
pRoots2 = pAllowedRoot [SCk, SCg, SCp, SCb, SCm]

-- ན
pRoot3 :: Parser Token
pRoot3 = pAllowedRoot [SCn]

-- ཙ
pRoot4 :: Parser Token
pRoot4 = pAllowedRoot [SCts]

pAllowedRoot :: [SubConsonant] -> Parser Token
pAllowedRoot allowed = do
  tok <- GP.pSubConsonant
  case tokenCanonical tok of
    TcSubConsonant sc
      | sc `elem` allowed -> pure tok
    _ -> MP.empty
