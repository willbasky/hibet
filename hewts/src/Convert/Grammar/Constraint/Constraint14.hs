{-
Tibetan spelling grammar 4.14 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint14 (pConstraint14) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token
    ( Consonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe, isJust)
import qualified Text.Megaparsec as MP

pConstraint14 :: Spelling -> Parser TibetanSyllable
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
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintUnicode14 parsePrefix parseRoot = do
    prefix <- mark Prefix parsePrefix
    root <- mark Root parseRoot
    vowel <- MP.optional (mark Vowel GP.pVowel)
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
-- the vowel slot differs - Wylie writes it in every syllable - and the stack
-- dot, which only Wylie has (REFACTOR-AST §8 A2).
--
-- A dot after the prefix breaks the stack, and the letter behind it is a root
-- of the book group even when a stack would subjoin that letter: g.yi is གཡི.
-- The same letter written without a dot stays the stack's subjoined letter
-- (gyi -> གྱི, the reading of structure 4.17). Which of the two a token is, is
-- read from the sign, not from the 'Spelling': a Tibetan subjoined sign is a
-- 'TcSubConsonant' and never reaches this parser as a root.
parseConstraintWylie14 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintWylie14 parsePrefix parseRoot = do
    prefix <- mark Prefix parsePrefix
    dotted <- MP.optional GP.pDot
    root <- mark Root $ do
        tok <- parseRoot
        if isJust dotted || not (GP.isSubjoinCandidate tok)
            then pure tok
            else MP.empty
    vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
    pure (prefix <> root <> vowel)
