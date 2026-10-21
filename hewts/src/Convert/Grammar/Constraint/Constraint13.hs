{-
Tibetan spelling grammar 4.13 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint13 (pConstraint13) where

import Convert.Grammar.Parser (Parser, SpellParser, mark, pJoinedRoot, vowelSlot)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token (Consonant (..), Token)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint13 :: SpellParser TibetanSyllable
pConstraint13 =
    MP.choice
        [ MP.try $
            parseConstraint13
                GP.pPrefixBa
                GP.pSuperfixSa
                pRoot
                (GP.pSubfixYa <|> GP.pSubfixRa)
        , MP.try $ parseConstraint13 GP.pPrefixBa GP.pSuperfixRa pRoot GP.pSubfixYa
        ]

parseConstraint13 ::
    Parser Token
    -> Parser Token
    -> Parser Token
    -> Parser Token
    -> SpellParser TibetanSyllable
parseConstraint13 parsePrefix parseSuperfix parseRoot parseSubfix = do
    prefix <- mark Prefix parsePrefix
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    subfix <- mark Subfix parseSubfix
    let stack = prefix <> superfix <> root <> subfix
    vowel <- vowelSlot stack [mark Vowel GP.pVowel]
    pure (stack <> vowel)

-- root group [ 'ཀ', 'ག' ]
pRoot :: Parser Token
pRoot = pJoinedRoot [Ck, Cg]
