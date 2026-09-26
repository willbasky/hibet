{-
Tibetan spelling grammar 4.11 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint11 (pConstraint11) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token
  ( Consonant (..)
  , SubConsonant (..)
  , Token
  , TokenCanonical (TcConsonant, TcSubConsonant)
  , tokenCanonical
  )
import Data.Maybe (fromMaybe)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint11 :: Parser TibetanWord
pConstraint11 =
  MP.choice
    [ MP.try $ parseConstraint11 GP.pPrefixBa GP.pSuperfixRa pRoots1
    , MP.try $ parseConstraint11 GP.pPrefixBa GP.pSuperfixLa pRoots2
    , MP.try $ parseConstraint11 GP.pPrefixBa GP.pSuperfixSa pRoots3
    ]

parseConstraint11 :: Parser Token -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraint11 parsePrefix parseSuperfix parseRoot = do
  prefix <- mark Prefix parsePrefix
  superfix <- mark Superfix parseSuperfix
  root <- mark Root parseRoot
  vowel <- MP.optional (mark Vowel GP.pVowel)
  pure (prefix <> superfix <> root <> fromMaybe [] vowel)

-- (1) root group [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཛ' ] with prefix བ under superfix ར
pRoots1 :: Parser Token
pRoots1 = pAllowedRoot [SCk, SCg, SCng, SCj, SCny, SCt, SCd, SCn, SCts, SCdz]

-- (2) root group [ 'ཏ', 'ད' ] with prefix བ under superfix ལ
pRoots2 :: Parser Token
pRoots2 = pAllowedRoot [SCt, SCd]

-- (3) root group [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ' ] with prefix བ under superfix ས
pRoots3 :: Parser Token
pRoots3 = pAllowedRoot [SCk, SCg, SCng, SCny, SCt, SCd, SCn, SCts]

pAllowedRoot :: [SubConsonant] -> Parser Token
pAllowedRoot allowed = do
  tok <- GP.pSubConsonant
  case tokenCanonical tok of
    TcSubConsonant sc
      | sc `elem` allowed -> pure tok
    _ -> MP.empty
