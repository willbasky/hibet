{-
Tibetan spelling grammar 4.8 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint08
    ( pConstraint08
    ) where

import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , markRoot
    , markSuperfix
    , markVowel
    , pJoinedRoot
    , vowelSlot
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Superfix (lSuperfixRoots, rSuperfixRoots, sSuperfixRoots)
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token (Token)
import qualified Text.Megaparsec as MP

-- | The root groups under the superfix: the joined sign in Tibetan, the full
-- letter in Wylie (the sign decides, see 'pJoinedRoot').
pConstraint08 :: SpellParser TibetanSyllable
pConstraint08 =
    MP.choice
        [ MP.try $ parseConstraint08 GP.pSuperfixRa (pJoinedRoot rSuperfixRoots)
        , MP.try $ parseConstraint08 GP.pSuperfixLa (pJoinedRoot lSuperfixRoots)
        , MP.try $ parseConstraint08 GP.pSuperfixSa (pJoinedRoot sSuperfixRoots)
        ]

parseConstraint08 ::
    Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraint08 parseSuperfix parseRoot = do
    superfix <- markSuperfix parseSuperfix
    root <- markRoot parseRoot
    vowel <- vowelSlot (superfix <> root) [markVowel GP.pVowel]
    pure (superfix <> root <> vowel)
