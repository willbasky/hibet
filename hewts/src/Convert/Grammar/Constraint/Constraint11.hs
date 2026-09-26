{-
Tibetan spelling grammar 4.11 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint11 (pConstraint11) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant, TcSubConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

pConstraint11 :: Spelling -> Parser TibetanWord
pConstraint11 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode11 GP.pPrefixBa GP.pSuperfixRa pRoots1
            , MP.try $ parseConstraintUnicode11 GP.pPrefixBa GP.pSuperfixLa pRoots2
            , MP.try $ parseConstraintUnicode11 GP.pPrefixBa GP.pSuperfixSa pRoots3
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie11 GP.pPrefixBa GP.pSuperfixRa pRoots1Wylie
            , MP.try $ parseConstraintWylie11 GP.pPrefixBa GP.pSuperfixLa pRoots2Wylie
            , MP.try $ parseConstraintWylie11 GP.pPrefixBa GP.pSuperfixSa pRoots3Wylie
            ]

parseConstraintUnicode11 ::
    Parser Token -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraintUnicode11 parsePrefix parseSuperfix parseRoot = do
    prefix <- mark Prefix parsePrefix
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.optional (mark Vowel GP.pVowel)
    pure (prefix <> superfix <> root <> fromMaybe mempty vowel)

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

-- The Wylie arm: the root under the superfix is a plain letter instead of the
-- joined sign, so each group has a full-letter twin; the vowel is always
-- written.

parseConstraintWylie11 ::
    Parser Token -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraintWylie11 parsePrefix parseSuperfix parseRoot = do
    prefix <- mark Prefix parsePrefix
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
    pure (prefix <> superfix <> root <> vowel)

pRoots1Wylie :: Parser Token
pRoots1Wylie = pAllowedRootWylie [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cts, Cdz]

pRoots2Wylie :: Parser Token
pRoots2Wylie = pAllowedRootWylie [Ct, Cd]

pRoots3Wylie :: Parser Token
pRoots3Wylie = pAllowedRootWylie [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cts]

pAllowedRootWylie :: [Consonant] -> Parser Token
pAllowedRootWylie allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
