{-
Tibetan spelling grammar 4.11 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint11 (pConstraint11) where

import Convert.Grammar.Parser (Parser, SpellParser, mark, pJoinedRoot, vowelSlot)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token (Consonant (..), Token)
import qualified Text.Megaparsec as MP

pConstraint11 :: SpellParser TibetanSyllable
pConstraint11 =
    MP.choice
        [ MP.try $ parseConstraint11 GP.pPrefixBa GP.pSuperfixRa pRoots1
        , MP.try $ parseConstraint11 GP.pPrefixBa GP.pSuperfixLa pRoots2
        , MP.try $ parseConstraint11 GP.pPrefixBa GP.pSuperfixSa pRoots3
        ]

parseConstraint11 ::
    Parser Token -> Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraint11 parsePrefix parseSuperfix parseRoot = do
    prefix <- mark Prefix parsePrefix
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    let stack = prefix <> superfix <> root
    vowel <- vowelSlot stack [mark Vowel GP.pVowel]
    pure (stack <> vowel)

-- (1) root group [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཛ' ] with prefix བ under superfix ར
pRoots1 :: Parser Token
pRoots1 = pJoinedRoot [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cts, Cdz]

-- (2) root group [ 'ཏ', 'ད' ] with prefix བ under superfix ལ
pRoots2 :: Parser Token
pRoots2 = pJoinedRoot [Ct, Cd]

-- (3) root group [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ' ] with prefix བ under superfix ས
pRoots3 :: Parser Token
pRoots3 = pJoinedRoot [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cts]
