{-
Tibetan spelling grammar 4.13 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint13 (pConstraint13) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Data.Maybe (fromMaybe)
import Convert.Token
  ( SubConsonant (..)
  , Token
  , TokenCanonical (TcSubConsonant)
  , tokenCanonical
  )
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint13 :: Parser TibetanWord
pConstraint13 =
  MP.choice
    [ MP.try $ parseConstraint13 GP.pPrefixBa GP.pSuperfixSa (GP.pSubfixYa <|> GP.pSubfixRa)
    , MP.try $ parseConstraint13 GP.pPrefixBa GP.pSuperfixRa GP.pSubfixYa
    ]

parseConstraint13 :: Parser Token -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraint13 parsePrefix parseSuperfix parseSubfix = do
  prefix <- mark Prefix parsePrefix
  superfix <- mark Superfix parseSuperfix
  root <- mark Root pRoot
  subfix <- mark Subfix parseSubfix
  vowel <- MP.optional (mark Vowel GP.pVowel)
  pure (prefix <> superfix <> root <> subfix <> fromMaybe mempty vowel)

-- root group [ 'ཀ', 'ག' ]
pRoot :: Parser Token
pRoot = pAllowedRoot [SCk, SCg]

pAllowedRoot :: [SubConsonant] -> Parser Token
pAllowedRoot allowed = do
  tok <- GP.pSubConsonant
  case tokenCanonical tok of
    TcSubConsonant sc
      | sc `elem` allowed -> pure tok
    _ -> MP.empty
