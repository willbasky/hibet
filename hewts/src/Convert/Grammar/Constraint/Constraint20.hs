{-
Tibetan spelling grammar 4.20 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint20
    ( pConstraint20
    ) where

import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , isJoinedAt
    , isWylieSign
    , liftP
    , markImplicitVowel
    , markRoot
    , markVowel
    , placeRoot
    , placeSuffix
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token
    )
import qualified Text.Megaparsec as MP

-- | Rule 4.20: the a-chung that closes a word.
--
-- The book knows it as a root here, and a root it stays when a written vowel
-- or a second root stands with it (@ཧིའེ@, @'ang@). A bare a-chung is the
-- word's last suffix (@མངའ@, @mnga'@) - our decision where the book is silent
-- (REFACTOR-AST §8 A), named here rather than guessed by the renderer. The
-- Wylie implicit @a@ is no written vowel: it is what an unwritten vowel spells,
-- so it leaves the a-chung bare too.
--
-- The role is named before the a-chung is placed: only the first sign behind it
-- decides, so a lookahead over the same signs reads it first, and the tail is
-- then marked for real. The sign of the a-chung itself says which spelling the
-- tail is written in.
pConstraint20 :: SpellParser TibetanSyllable
pConstraint20 = do
    tok <- liftP (MP.satisfy (GP.isSpecificConsonant C') MP.<?> "A root འ")
    rootish <- MP.option False (MP.lookAhead (True <$ liftP rootHeads))
    a <- if rootish then placeRoot tok else placeSuffix tok
    trailer <- MP.option mempty (MP.choice (tails (isWylieSign tok)))
    pure (a <> trailer)
    where
        -- The book's own shapes: འ takes a vowel, or a second root (Def 4.10's
        -- "consonant alphabet with a consonant alphabet"), so ང and མ stand as
        -- roots behind it.
        rootHeads :: Parser Token
        rootHeads =
            MP.choice
                [ GP.pVowel
                , MP.satisfy (isJoinedAt SCng Cng)
                , MP.satisfy (isJoinedAt SCm Cm)
                ]

        tails :: Bool -> [SpellParser TibetanSyllable]
        tails wylie =
            [markVowel GP.pVowel]
                ++ [markImplicitVowel GP.pImplicitA | wylie]
                ++ [ markRoot (MP.satisfy (isJoinedAt SCng Cng))
                   , markRoot (MP.satisfy (isJoinedAt SCm Cm))
                   ]
