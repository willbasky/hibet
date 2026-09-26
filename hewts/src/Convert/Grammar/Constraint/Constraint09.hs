{-
Tibetan spelling grammar 4.9 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint09 (pConstraint09) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token
  ( Consonant (..)
  , Token
  , TokenCanonical (TcConsonant)
  , tokenCanonical
  )

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint09 :: Spelling -> Parser TibetanWord
pConstraint09 spelling =
  MP.choice
    [ MP.try $ parseConstraint09 spelling GP.pSubfixWa pRootSubfixWa
    , MP.try $ parseConstraint09 spelling GP.pSubfixYa pRootSubfixYa
    , MP.try $ parseConstraint09 spelling GP.pSubfixRa pRootSubfixRa
    , MP.try $ parseConstraint09 spelling GP.pSubfixLa pRootSubfixLa
    ]

parseConstraint09 :: Spelling -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraint09 spelling parseSubfix parseRoot = do
  root <- mark Root parseRoot
  subfix <- mark Subfix parseSubfix
  vowel <- MP.optional (vowelSlot spelling)
  pure (root <> subfix <> fromMaybe mempty vowel)

-- Roots above subfix 'ཝ' are [ 'ཀ', 'ཁ', 'ག', 'ཉ', 'ད', 'ཚ', 'ཞ', 'ཟ', 'ར', 'ལ', 'ཤ', 'ཧ' ]
pRootSubfixWa :: Parser Token
pRootSubfixWa = pAllowedRoot [Ck, Ckh, Cg, Cny, Cd, Ctsh, Czh, Cz, Cr, Cl, Csh, Ch]

-- Roots above subfix 'ཡ' are [ 'ཀ', 'ཁ', 'ག', 'པ', 'ཕ', 'བ', 'མ' ]
pRootSubfixYa :: Parser Token
pRootSubfixYa = pAllowedRoot [Ck, Ckh, Cg, Cp, Cph, Cb, Cm]

-- Roots above subfix 'ར' are [ 'ཀ', 'ཁ', 'ག', 'ཏ', 'ཐ', 'ད', 'པ', 'ཕ', 'བ', 'མ', 'ས', 'ཧ' ]
pRootSubfixRa :: Parser Token
pRootSubfixRa = pAllowedRoot [Ck, Ckh, Cg, Ct, Cth, Cd, Cp, Cph, Cb, Cm, Cs, Ch]

-- Roots above subfix 'ལ' are [ 'ཀ', 'ག', 'བ', 'ཟ', 'ར', 'ས' ]
pRootSubfixLa :: Parser Token
pRootSubfixLa = pAllowedRoot [Ck, Cg, Cb, Cz, Cr, Cs]

pAllowedRoot :: [Consonant] -> Parser Token
pAllowedRoot allowed = do
  tok <- GP.pRootConsonant
  case tokenCanonical tok of
    TcConsonant c
      | c `elem` allowed -> pure tok
    _ -> MP.empty
