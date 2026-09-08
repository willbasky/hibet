{-
Tibetan spelling grammar 4.9 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint09 (pConstraint09) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Token
  ( Consonant (..)
  , Token
  , TokenCanonical (TcConsonant)
  , tokenCanonical
  )

import Data.Maybe (maybeToList)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint09 :: Parser [Token]
pConstraint09 =
  MP.choice
    [ MP.try $ parseConstraint09 GP.pSubfixWa pRootSubfixWa
    , MP.try $ parseConstraint09 GP.pSubfixYa pRootSubfixYa
    , MP.try $ parseConstraint09 GP.pSubfixRa pRootSubfixRa
    , MP.try $ parseConstraint09 GP.pSubfixLa pRootSubfixLa
    ]

parseConstraint09 :: Parser Token -> Parser Token -> Parser [Token]
parseConstraint09 parseSubfix parseRoot = do
  root <- parseRoot
  subfix <- parseSubfix
  vowel <- MP.optional GP.pVowel
  pure $ [root, subfix] <> maybeToList vowel

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
