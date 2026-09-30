{-
Tibetan spelling grammar 4.10 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint10 (pConstraint10) where

import Convert.Grammar.Parser (Parser, SpellParser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, markS)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant, TcSubConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint10 :: Spelling -> SpellParser TibetanSyllable
pConstraint10 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode10 GP.pSuperfixRa pRoots1 GP.pSubfixYa
            , MP.try $
                parseConstraintUnicode10 GP.pSuperfixSa pRoots2 (GP.pSubfixYa <|> GP.pSubfixRa)
            , MP.try $ parseConstraintUnicode10 GP.pSuperfixSa pRoot3 GP.pSubfixRa
            , MP.try $ parseConstraintUnicode10 GP.pSuperfixRa pRoot4 GP.pSubfixWa
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie10 GP.pSuperfixRa pRoots1Wylie GP.pSubfixYaWylie
            , MP.try $
                parseConstraintWylie10
                    GP.pSuperfixSa
                    pRoots2Wylie
                    (GP.pSubfixYaWylie <|> GP.pSubfixRaWylie)
            , MP.try $ parseConstraintWylie10 GP.pSuperfixSa pRoot3Wylie GP.pSubfixRaWylie
            , MP.try $ parseConstraintWylie10 GP.pSuperfixRa pRoot4Wylie GP.pSubfixWaWylie
            ]

parseConstraintUnicode10 ::
    Parser Token -> Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraintUnicode10 parseSuperfix parseRoot parseSubfix = do
    superfix <- markS Superfix parseSuperfix
    root <- markS Root parseRoot
    subfix <- markS Subfix parseSubfix
    vowel <- MP.optional (markS Vowel GP.pVowel)
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

-- The Wylie arm: the root under the superfix and the subfix are plain letters,
-- so each group has a full-letter twin and the vowel is always written.

parseConstraintWylie10 ::
    Parser Token -> Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraintWylie10 parseSuperfix parseRoot parseSubfix = do
    superfix <- markS Superfix parseSuperfix
    root <- markS Root parseRoot
    subfix <- markS Subfix parseSubfix
    vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
    pure (superfix <> root <> subfix <> vowel)

pRoots1Wylie :: Parser Token
pRoots1Wylie = pAllowedRootWylie [Ck, Cg, Cm]

pRoots2Wylie :: Parser Token
pRoots2Wylie = pAllowedRootWylie [Ck, Cg, Cp, Cb, Cm]

pRoot3Wylie :: Parser Token
pRoot3Wylie = pAllowedRootWylie [Cn]

pRoot4Wylie :: Parser Token
pRoot4Wylie = pAllowedRootWylie [Cts]

-- | A full consonant letter from a group (Wylie spells stacked letters in full).
pAllowedRootWylie :: [Consonant] -> Parser Token
pAllowedRootWylie allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
