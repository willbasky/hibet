{-
Tibetan spelling grammar 4.14 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint14 (pConstraint14) where

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
import qualified Text.Megaparsec as MP

pConstraint14 :: Spelling -> SpellParser TibetanSyllable
pConstraint14 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode14 GP.pPrefixGa pRoots1
            , MP.try $ parseConstraintUnicode14 GP.pPrefixDa pRoots2
            , MP.try $ parseConstraintUnicode14 GP.pPrefixBa pRoots3
            , MP.try $ parseConstraintUnicode14 GP.pPrefixMa pRoots4
            , MP.try $ parseConstraintUnicode14 GP.pPrefixA pRoots5
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie14 GP.pPrefixGa pRoots1
            , MP.try $ parseConstraintWylie14 GP.pPrefixDa pRoots2
            , MP.try $ parseConstraintWylie14 GP.pPrefixBa pRoots3
            , MP.try $ parseConstraintWylie14 GP.pPrefixMa pRoots4
            , MP.try $ parseConstraintWylie14 GP.pPrefixA pRoots5
            ]

parseConstraintUnicode14 ::
    Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraintUnicode14 parsePrefix parseRoot = do
    prefix <- markS Prefix parsePrefix
    root <- markS Root parseRoot
    vowel <- MP.optional (markS Vowel GP.pVowel)
    pure (prefix <> root <> fromMaybe mempty vowel)

-- (1) root group [ 'ཅ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཞ', 'ཟ', 'ཡ', 'ཤ', 'ས' ] with prefix ག
pRoots1 :: Parser Token
pRoots1 = pAllowedRoot [Cc, Cny, Ct, Cd, Cn, Cts, Czh, Cz, Cy, Csh, Cs]

-- (2) root group [ 'ཀ', 'ག', 'ང', 'པ', 'བ', 'མ' ] with prefix ད
pRoots2 :: Parser Token
pRoots2 = pAllowedRoot [Ck, Cg, Cng, Cp, Cb, Cm]

-- (3) root group [ 'ཀ', 'ག', 'ཅ', 'ཏ', 'ད', 'ཙ', 'ཞ', 'ཟ', 'ཤ', 'ས' ] with prefix བ
pRoots3 :: Parser Token
pRoots3 = pAllowedRoot [Ck, Cg, Cc, Ct, Cd, Cts, Czh, Cz, Csh, Cs]

-- (4) root group [ 'ཁ', 'ག', 'ང', 'ཆ', 'ཇ', 'ཉ', 'ཐ', 'ད', 'ན', 'ཚ', 'ཛ' ] with prefix མ
pRoots4 :: Parser Token
pRoots4 = pAllowedRoot [Ckh, Cg, Cng, Cch, Cj, Cny, Cth, Cd, Cn, Ctsh, Cdz]

-- (5) root group [ 'ཁ', 'ག', 'ཆ', 'ཇ', 'ཐ', 'ད', 'ཕ', 'བ', 'ཚ', 'ཛ' ] with prefix འ
pRoots5 :: Parser Token
pRoots5 = pAllowedRoot [Ckh, Cg, Cch, Cj, Cth, Cd, Cph, Cb, Ctsh, Cdz]

pAllowedRoot :: [Consonant] -> Parser Token
pAllowedRoot allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty

-- The Wylie arm: the prefix and the root are full letters either way, so only
-- the vowel slot differs - Wylie writes it in every syllable.

parseConstraintWylie14 ::
    Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraintWylie14 parsePrefix parseRoot = do
    prefix <- markS Prefix parsePrefix
    root <- markS Root parseRoot
    vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
    pure (prefix <> root <> vowel)
