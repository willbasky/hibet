{-
Tibetan spelling grammar 4.13 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint13 (pConstraint13) where

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

pConstraint13 :: Spelling -> SpellParser TibetanSyllable
pConstraint13 = \case
    Tibetan ->
        MP.choice
            [ MP.try $
                parseConstraintUnicode13
                    GP.pPrefixBa
                    GP.pSuperfixSa
                    pRoot
                    (GP.pSubfixYa <|> GP.pSubfixRa)
            , MP.try $ parseConstraintUnicode13 GP.pPrefixBa GP.pSuperfixRa pRoot GP.pSubfixYa
            ]
    Wylie ->
        MP.choice
            [ MP.try $
                parseConstraintWylie13
                    GP.pPrefixBa
                    GP.pSuperfixSa
                    pRootWylie
                    (GP.pSubfixYaWylie <|> GP.pSubfixRaWylie)
            , MP.try $
                parseConstraintWylie13 GP.pPrefixBa GP.pSuperfixRa pRootWylie GP.pSubfixYaWylie
            ]

parseConstraintUnicode13 ::
    Parser Token
    -> Parser Token
    -> Parser Token
    -> Parser Token
    -> SpellParser TibetanSyllable
parseConstraintUnicode13 parsePrefix parseSuperfix parseRoot parseSubfix = do
    prefix <- markS Prefix parsePrefix
    superfix <- markS Superfix parseSuperfix
    root <- markS Root parseRoot
    subfix <- markS Subfix parseSubfix
    vowel <- MP.optional (markS Vowel GP.pVowel)
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

-- The Wylie arm: the root under the superfix is a full letter and the subfix
-- is a plain letter, so each has a full-letter twin; the vowel is always
-- written.

parseConstraintWylie13 ::
    Parser Token
    -> Parser Token
    -> Parser Token
    -> Parser Token
    -> SpellParser TibetanSyllable
parseConstraintWylie13 parsePrefix parseSuperfix parseRoot parseSubfix = do
    prefix <- markS Prefix parsePrefix
    superfix <- markS Superfix parseSuperfix
    root <- markS Root parseRoot
    subfix <- markS Subfix parseSubfix
    vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
    pure (prefix <> superfix <> root <> subfix <> vowel)

pRootWylie :: Parser Token
pRootWylie = pAllowedRootWylie [Ck, Cg]

pAllowedRootWylie :: [Consonant] -> Parser Token
pAllowedRootWylie allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
