{-
Tibetan spelling grammar 4.20 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint20
    ( pConstraint20
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token
    )
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Text.Megaparsec as MP

-- | Rule 4.20: the a-chung that closes a word.
--
-- The book knows it as a root here, and a root it stays when a written vowel
-- or a second root stands with it (@ཧིའེ@, @'ang@). A bare a-chung is the
-- word's last suffix (@མངའ@, @mnga'@) - our decision where the book is silent
-- (REFACTOR-AST §8 A), named here rather than guessed by the renderer. The
-- Wylie implicit @a@ is no written vowel: it is what an unwritten vowel spells,
-- so it leaves the a-chung bare too.
pConstraint20 :: Spelling -> Parser TibetanSyllable
pConstraint20 = \case
    -- The book's own shapes: འ takes a vowel, or a second root (Def 4.10's
    -- "consonant alphabet with a consonant alphabet"), so ང and མ stand as
    -- roots behind it.
    Tibetan ->
        aChung . fmap (fromMaybe mempty) . MP.optional $
            MP.choice
                [ tailMark Vowel GP.pVowel
                , tailMark
                    Root
                    (MP.satisfy (GP.isSpecificSubConsonant SCng) MP.<?> "A subConsonant ང")
                , tailMark
                    Root
                    (MP.satisfy (GP.isSpecificSubConsonant SCm) MP.<?> "A subConsonant མ")
                ]
    Wylie ->
        aChung $
            MP.choice
                [ tailMark Vowel GP.pVowel
                , tailMark ImplicitVowel GP.pImplicitA
                , tailMark
                    Root
                    (MP.satisfy (GP.isSpecificConsonant Cng) MP.<?> "A second root ང")
                , tailMark
                    Root
                    (MP.satisfy (GP.isSpecificConsonant Cm) MP.<?> "A second root མ")
                ]
    where
        aChung :: Parser (Seq (Position, Token)) -> Parser TibetanSyllable
        aChung tailParser = do
            tok <- MP.satisfy (GP.isSpecificConsonant C') MP.<?> "A root འ"
            marks <- tailParser
            pure ((roleOf marks, tok) Seq.<| marks)

        -- The place the sign behind the a-chung takes, as the other structures
        -- state it with 'mark'.
        tailMark :: Position -> Parser Token -> Parser (Seq (Position, Token))
        tailMark pos = fmap (Seq.singleton . (pos,))

        -- A written vowel or a second root keeps the a-chung a root; anything else
        -- (the Wylie implicit @a@, or no sign at all) leaves it bare.
        roleOf :: Seq (Position, Token) -> Position
        roleOf marks = case Seq.lookup 0 marks of
            Just (Vowel, _) -> Root
            Just (Root, _) -> Root
            _ -> Suffix
