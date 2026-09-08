{-
Tibetan spelling grammar 4.12 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint12 (pConstraint12) where

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

pConstraint12 :: Parser [Token]
pConstraint12 =
  MP.choice
    [ MP.try $ parseConstraint12 GP.pPrefixDa pRoots1 GP.pSubfixYa
    , MP.try $ parseConstraint12 GP.pPrefixDa pRoots2 GP.pSubfixRa
    , MP.try $ parseConstraint12 GP.pPrefixBa pRoots3 GP.pSubfixYa
    , MP.try $ parseConstraint12 GP.pPrefixBa pRoots4 GP.pSubfixRa
    , MP.try $ parseConstraint12 GP.pPrefixBa pRoots5 GP.pSubfixLa
    , MP.try $ parseConstraint12 GP.pPrefixMa pRoots6 (GP.pSubfixYa <|> GP.pSubfixRa)
    , MP.try $ parseConstraint12 GP.pPrefixA pRoots7 GP.pSubfixYa
    , MP.try $ parseConstraint12 GP.pPrefixA pRoots8 GP.pSubfixRa
    ]

parseConstraint12 :: Parser Token -> Parser Token -> Parser Token -> Parser [Token]
parseConstraint12 parsePrefix parseRoot parseSubfix = do
  prefix <- parsePrefix
  root <- parseRoot
  subfix <- parseSubfix
  vowel <- MP.optional GP.pVowel
  pure $ [prefix, root, subfix] <> maybeToList vowel

-- (1) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] with prefix ད under subfix ཡ
pRoots1 :: Parser Token
pRoots1 = pAllowedRoot [Ck, Cg, Cp, Cb, Cm]

-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ' ] with prefix ད under subfix ར
pRoots2 :: Parser Token
pRoots2 = pAllowedRoot [Ck, Cg, Cp, Cb]

-- (3) root group [ 'ཀ', 'ག' ] with prefix བ under subfix ཡ
pRoots3 :: Parser Token
pRoots3 = pAllowedRoot [Ck, Cg]

-- (4) root group [ 'ཀ', 'ག', 'ས' ] with prefix བ under subfix ར
pRoots4 :: Parser Token
pRoots4 = pAllowedRoot [Ck, Cg, Cs]

-- (5) root group [ 'ཀ', 'ཟ', 'ར', 'ས' ] with prefix བ under subfix ལ
pRoots5 :: Parser Token
pRoots5 = pAllowedRoot [Ck, Cz, Cr, Cs]

-- (6) root group [ 'ཁ', 'ག' ] with prefix མ under subfix ཡ or ར
pRoots6 :: Parser Token
pRoots6 = pAllowedRoot [Ckh, Cg]

-- (7) root group [ 'ཁ', 'ག', 'ཕ', 'བ' ] with prefix འ under subfix ཡ
pRoots7 :: Parser Token
pRoots7 = pAllowedRoot [Ckh, Cg, Cph, Cb]

-- (8) root group [ 'ཁ', 'ག', 'ད', 'ཕ', 'བ' ] with prefix འ under subfix ར
pRoots8 :: Parser Token
pRoots8 = pAllowedRoot [Ckh, Cg, Cd, Cph, Cb]

pAllowedRoot :: [Consonant] -> Parser Token
pAllowedRoot allowed = do
  tok <- GP.pConsonant
  case tokenCanonical tok of
    TcConsonant c
      | c `elem` allowed -> pure tok
    _ -> MP.empty
