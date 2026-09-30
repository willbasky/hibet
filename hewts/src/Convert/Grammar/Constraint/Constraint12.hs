{-
Tibetan spelling grammar 4.12 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint12 (pConstraint12) where

import Convert.Grammar.Parser (Parser, SpellParser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, markS)
import Convert.Token
    ( Consonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint12 :: Spelling -> SpellParser TibetanSyllable
pConstraint12 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode12 GP.pPrefixDa pRoots1 GP.pSubfixYa
            , MP.try $ parseConstraintUnicode12 GP.pPrefixDa pRoots2 GP.pSubfixRa
            , MP.try $ parseConstraintUnicode12 GP.pPrefixBa pRoots3 GP.pSubfixYa
            , MP.try $ parseConstraintUnicode12 GP.pPrefixBa pRoots4 GP.pSubfixRa
            , MP.try $ parseConstraintUnicode12 GP.pPrefixBa pRoots5 GP.pSubfixLa
            , MP.try $
                parseConstraintUnicode12 GP.pPrefixMa pRoots6 (GP.pSubfixYa <|> GP.pSubfixRa)
            , MP.try $ parseConstraintUnicode12 GP.pPrefixA pRoots7 GP.pSubfixYa
            , MP.try $ parseConstraintUnicode12 GP.pPrefixA pRoots8 GP.pSubfixRa
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie12 GP.pPrefixDa pRoots1 GP.pSubfixYaWylie
            , MP.try $ parseConstraintWylie12 GP.pPrefixDa pRoots2 GP.pSubfixRaWylie
            , MP.try $ parseConstraintWylie12 GP.pPrefixBa pRoots3 GP.pSubfixYaWylie
            , MP.try $ parseConstraintWylie12 GP.pPrefixBa pRoots4 GP.pSubfixRaWylie
            , MP.try $ parseConstraintWylie12 GP.pPrefixBa pRoots5 GP.pSubfixLaWylie
            , MP.try $
                parseConstraintWylie12
                    GP.pPrefixMa
                    pRoots6
                    (GP.pSubfixYaWylie <|> GP.pSubfixRaWylie)
            , MP.try $ parseConstraintWylie12 GP.pPrefixA pRoots7 GP.pSubfixYaWylie
            , MP.try $ parseConstraintWylie12 GP.pPrefixA pRoots8 GP.pSubfixRaWylie
            ]

parseConstraintUnicode12 ::
    Parser Token -> Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraintUnicode12 parsePrefix parseRoot parseSubfix = do
    prefix <- markS Prefix parsePrefix
    root <- markS Root parseRoot
    subfix <- markS Subfix parseSubfix
    vowel <- MP.optional (markS Vowel GP.pVowel)
    pure (prefix <> root <> subfix <> fromMaybe mempty vowel)

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

-- The Wylie arm: the root above the subfix is a full letter either way and the
-- groups are shared; only the subfix is a plain letter here and the vowel is
-- always written.

parseConstraintWylie12 ::
    Parser Token -> Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraintWylie12 parsePrefix parseRoot parseSubfix = do
    prefix <- markS Prefix parsePrefix
    root <- markS Root parseRoot
    subfix <- markS Subfix parseSubfix
    vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
    pure (prefix <> root <> subfix <> vowel)
