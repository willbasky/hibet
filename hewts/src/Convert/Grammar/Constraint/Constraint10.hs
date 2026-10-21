{-
Tibetan spelling grammar 4.10 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint10 (pConstraint10) where

import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , markRoot
    , markSubfix
    , markSuperfix
    , markVowel
    , pJoinedRoot
    , vowelSlot
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token (Consonant (..), Token)
import Data.Maybe (fromMaybe)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint10 :: SpellParser TibetanSyllable
pConstraint10 =
    MP.choice
        [ MP.try $ parseConstraint10 GP.pSuperfixRa pRoots1 GP.pSubfixYa
        , MP.try $
            parseConstraint10 GP.pSuperfixSa pRoots2 (GP.pSubfixYa <|> GP.pSubfixRa)
        , MP.try $ parseConstraint10 GP.pSuperfixSa pRoot3 GP.pSubfixRa
        , MP.try $ parseConstraint10 GP.pSuperfixRa pRoot4 GP.pSubfixWa
        ]

parseConstraint10 ::
    Parser Token -> Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraint10 parseSuperfix parseRoot parseSubfix = do
    superfix <- markSuperfix parseSuperfix
    root <- markRoot parseRoot
    subfix <- markSubfix parseSubfix
    -- REFACTOR-AST §8 A3: the book stops 4.10 at one subjoined sign, but
    -- སྒྲྭ is a superfix over a root with two (ས + ྒ + ྲ + ྭ). The form is our
    -- addition; the root groups stay 4.10's, and the second letter is one of
    -- the subjoins 4.9 allows in second place (@l@ never is).
    second <-
        MP.optional (markSubfix (GP.pSubfixYa <|> GP.pSubfixRa <|> GP.pSubfixWa))
    let stack = superfix <> root <> subfix <> fromMaybe mempty second
    vowel <- vowelSlot stack [markVowel GP.pVowel]
    pure (stack <> vowel)

-- (1) root group [ 'ཀ', 'ག', 'མ' ] under superfix ར and above subfix ཡ
pRoots1 :: Parser Token
pRoots1 = pJoinedRoot [Ck, Cg, Cm]

-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] under superfix ས and above subfix ཡ or ར
pRoots2 :: Parser Token
pRoots2 = pJoinedRoot [Ck, Cg, Cp, Cb, Cm]

-- ན
pRoot3 :: Parser Token
pRoot3 = pJoinedRoot [Cn]

-- ཙ
pRoot4 :: Parser Token
pRoot4 = pJoinedRoot [Cts]
