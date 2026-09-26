{-
Tibetan spelling grammar 4.10 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint10 (pConstraint10) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token
  ( SubConsonant (..)
  , Token
  , TokenCanonical (TcSubConsonant)
  , tokenCanonical
  )
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint10 :: Spelling -> Parser TibetanWord
pConstraint10 spelling =
  MP.choice
    [ MP.try $ parseConstraint10 spelling GP.pSuperfixRa pRoots1 GP.pSubfixYa
    , MP.try $ parseConstraint10 spelling GP.pSuperfixSa pRoots2 (GP.pSubfixYa <|> GP.pSubfixRa)
    , MP.try $ parseConstraint10 spelling GP.pSuperfixSa pRoot3 GP.pSubfixRa
    , MP.try $ parseConstraint10 spelling GP.pSuperfixRa pRoot4 GP.pSubfixWa
    ]

parseConstraint10 :: Spelling -> Parser Token -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraint10 spelling parseSuperfix parseRoot parseSubfix = do
  superfix <- mark Superfix parseSuperfix
  root <- mark Root parseRoot
  subfix <- mark Subfix parseSubfix
  vowel <- MP.optional (vowelSlot spelling)
  pure (superfix <> root <> subfix <> fromMaybe mempty vowel)

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
