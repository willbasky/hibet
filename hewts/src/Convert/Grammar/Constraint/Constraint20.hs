{-
Tibetan spelling grammar 4.20 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint20
    ( pConstraint20
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

pConstraint20 :: Spelling -> Parser TibetanSyllable
pConstraint20 = \case
    Tibetan -> do
        root <- mark Root (MP.satisfy (GP.isSpecificConsonant C') MP.<?> "A root འ")
        -- འ takes either a vowel, or a second root (Def 4.10's special case:
        -- "a consonant alphabet with a consonant alphabet"), so ང and མ are roots here
        vowelA <-
            MP.optional $
                MP.choice
                    [ mark Vowel GP.pVowel
                    , mark
                        Root
                        (MP.satisfy (GP.isSpecificSubConsonant SCng) MP.<?> "A subConsonant ང")
                    , mark
                        Root
                        (MP.satisfy (GP.isSpecificSubConsonant SCm) MP.<?> "A subConsonant མ")
                    ]
        pure (root <> fromMaybe mempty vowelA)
    Wylie -> do
        root <- mark Root (MP.satisfy (GP.isSpecificConsonant C') MP.<?> "A root འ")
        -- The Wylie arm: འáng is written "'ang", the a of the a-chung spelling
        -- fills the vowel slot as an implicit @a@, and the second root ང or མ
        -- is a full letter.
        vowelA <-
            MP.choice
                [ mark Vowel GP.pVowel
                , mark ImplicitVowel GP.pImplicitA
                , mark Root (MP.satisfy (GP.isSpecificConsonant Cng) MP.<?> "A second root ང")
                , mark Root (MP.satisfy (GP.isSpecificConsonant Cm) MP.<?> "A second root མ")
                ]
        pure (root <> vowelA)
