{-
Tibetan spelling grammar 4.9 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint09 (pConstraint09) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token
    ( Consonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

pConstraint09 :: Spelling -> Parser TibetanSyllable
pConstraint09 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode09 GP.pSubfixWa pRootSubfixWa
            , MP.try $ parseConstraintUnicode09 GP.pSubfixYa pRootSubfixYa
            , MP.try $ parseConstraintUnicode09 GP.pSubfixRa pRootSubfixRa
            , MP.try $ parseConstraintUnicode09 GP.pSubfixLa pRootSubfixLa
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie09 GP.pSubfixWaWylie pRootSubfixWa
            , MP.try $ parseConstraintWylie09 GP.pSubfixYaWylie pRootSubfixYa
            , MP.try $ parseConstraintWylie09 GP.pSubfixRaWylie pRootSubfixRa
            , MP.try $ parseConstraintWylie09 GP.pSubfixLaWylie pRootSubfixLa
            ]

parseConstraintUnicode09 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintUnicode09 parseSubfix parseRoot = do
    root <- mark Root parseRoot
    subfix <- mark Subfix parseSubfix
    vowel <- MP.optional (mark Vowel GP.pVowel)
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

-- The Wylie arm: the root above the subfix is the same full letter either way
-- - only the subfix itself is a plain letter here, so both arms share the
-- root groups - and the vowel is always written.

parseConstraintWylie09 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintWylie09 parseSubfix parseRoot = do
    root <- mark Root parseRoot
    subfix <- mark Subfix parseSubfix
    vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
    pure (root <> subfix <> vowel)
