{- Tibetan spelling grammar 4.21 (token parser variant)

The whole generic word of 'Convert.Grammar.Structure.pStructure38' - whatever
none of the book's 37 structures spell. One stack is the base consonant, the
letters subjoined to it, and the vowel that ends it; when a stack cannot reach
a vowel it keeps only its first consonant and everything after it begins a
fresh stack (jsewts's own backtrack). The cursor (caret, @^@) is transparent
while the stack subjoins and prints as a final mark between the subfixes and
the vowel; the forced-join sign @+@ (Wylie syntax only) joins the letter after
it below the stack; a word may open (ug -> ཨུག) and continue (g.yon -> གཡོན)
on anything outside the stack letters. A @+@ the stack already closed still
reads as a join: the letter after it stays in the same word (u+e -> ཨེུ,
rH+e -> རཿེ), as the references read it.

No spelling switch: the lattice - which bare letters subjoin, which vowels a
stack absorbs - is decided per token, by 'tokenSource', not by the 'Spelling'
of the parse. The parity runs Wylie token lists under 'Tibetan' as well, and
there is nowhere a @+@ or a dot could be spelled from a Tibetan stream, so the
same parser covers both arms, exactly as the old 'Convert.Grammar.Stack'
ignored its own 'Spelling' argument.
-}

module Convert.Grammar.Constraint.Constraint21
    ( pConstraint21First
    , pConstraint21Rest
    ) where

import Control.Monad (void)
import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable
    ( Position (..)
    , TibetanSyllable
    , caretMark
    , mark
    , subfixMarks
    )
import Convert.Token (Token)
import qualified Data.Sequence as Seq
import qualified Text.Megaparsec as MP

-- | The word lead: a consonant-led or a vowel-led stack (the old pStack @t0@).
-- The lead stack is the only one that may open on a prefix letter (bsgribs ->
-- བ), because only a word's first stack is a prefix slot.
pConstraint21First :: Parser TibetanSyllable
pConstraint21First =
    MP.choice
        [ pStackBody True
        , mark Vowel GP.pVowelAny
        ]

-- | Every way the word can continue: another consonant-led stack, a lone
-- vowel, a lone final mark (a caret among them), a subjoined letter the
-- previous stack could not keep, a forced join the previous stack closed too
-- soon to swallow (@u+e@ -> ཨེུ, @rH+e@ -> རཿེ), and a stack-breaking dot; a
-- dotted stack still spells as one word (g.yon -> གཡོན).
pConstraint21Rest :: Parser TibetanSyllable
pConstraint21Rest =
    MP.choice
        [ pDotBreak
        , pForcedJoin
        , mark Vowel GP.pVowelAny
        , mark Final GP.pFinal
        , mark Subfix GP.pSubConsonant
        , pStackBody False
        ]

-- | A dot that splits one stack into two (g.yon): only a dot followed by a
-- consonant reads this way, otherwise the word ends before it and the dot is
-- left for the sentence. Tibetan token streams never carry a dot.
pDotBreak :: Parser TibetanSyllable
pDotBreak =
    MP.try (GP.pDot <* MP.lookAhead (MP.satisfy GP.isConsonantToken))
        *> pure mempty

pStackBody :: Bool -> Parser TibetanSyllable
pStackBody atStart = do
    base <- GP.pConsonant
    next0 <- MP.lookAhead (MP.skipMany GP.pCaret *> MP.optional GP.pToken)
    let baseMark = Seq.singleton (markBase atStart base next0)
    MP.choice
        [ completeStack baseMark
        , pure baseMark
        ]
    where
        -- The subjoining run under the base is over and the stack next meets
        -- a vowel or a @+@: spell the whole stack - the base, the subjoined
        -- letters, the caret, and the whole tail. Fails and rolls back to the
        -- bare base when neither follows.
        completeStack :: TibetanSyllable -> Parser TibetanSyllable
        completeStack baseMark = MP.try $ do
            (subjoined, caret) <- GP.pSubjoinRun
            next <- MP.lookAhead (MP.optional GP.pToken)
            case next of
                Just t | GP.isVowelLike t || GP.isPlus t -> do
                    tailMarks <- pConsumeTail False
                    pure (baseMark <> subfixMarks subjoined <> caretMark caret <> tailMarks)
                _ -> MP.empty

-- | The rest of a complete stack - consecutive vowels (@a@ among them, the
-- letter that never prints), the caret and then finals, and forced subjoins
-- (@+X@, each possibly pulling its own subjoining run, g+mra). After a real
-- vowel the tail is over: a bare @a@ that follows (goang གོཨང) is the root ཨ
-- of a fresh stack, not the implicit vowel again, so the a-chen branch only
-- fires before the first vowel.
pConsumeTail :: Bool -> Parser TibetanSyllable
pConsumeTail vowelSeen =
    MP.choice
        [ pTailMark (mark Vowel GP.pVowelAny) True
        , pImplicitABranch
        , pTailMark (mark Final GP.pFinal) vowelSeen
        , pForcedJoinBranch
        , pure mempty
        ]
    where
        -- One more mark of the tail, then the rest of it under the flag this
        -- mark leaves for the continuation.
        pTailMark :: Parser TibetanSyllable -> Bool -> Parser TibetanSyllable
        pTailMark step flag =
            MP.try $ do
                marks <- step
                rest <- pConsumeTail flag
                pure (marks <> rest)

        -- The bare @a@ only fits before the first real vowel of the stack.
        pImplicitABranch :: Parser TibetanSyllable
        pImplicitABranch
            | vowelSeen = MP.empty
            | otherwise = pTailMark (mark ImplicitVowel GP.pImplicitA) False

        -- A forced join may itself be a vowel (@+e@): the tail after it then
        -- continues as after any real vowel.
        pForcedJoinBranch :: Parser TibetanSyllable
        pForcedJoinBranch = MP.try $ do
            marks <- pForcedJoin
            rest <- pConsumeTail (vowelSeen || startsWithVowel marks)
            pure (marks <> rest)

        -- Whether a forced join ended in a vowel (@+e@).
        startsWithVowel :: TibetanSyllable -> Bool
        startsWithVowel w = case Seq.lookup 0 w of
            Just (Vowel, _) -> True
            _ -> False

-- | The forced join sign @+@ (Wylie syntax only), with two readings: the
-- letter after it is dragged below the stack as a subfix together with its
-- own subjoining run ('pSubjoinBelow', g+mra -> གྨྲ), or - when the stack
-- already had its vowel - the sign is followed by that stack's vowel
-- (bru+e -> བྲེུ). The a-chen (s+a -> སྸ) is a consonant like any other, so
-- it falls into the join-below reading. The same parser reads a sign the
-- stack already closed, because the references read it the same way the open
-- stack does: the vowel or consonant after it still belongs to the same word
-- (u+e -> ཨེུ, rH+e -> རཿེ). 'pConstraint21Rest' reaches for it there.
pForcedJoin :: Parser TibetanSyllable
pForcedJoin = MP.try $ do
    void GP.pPlus
    MP.choice
        [ MP.try pSubjoinBelow
        , mark Vowel GP.pVowelAny
        ]
    where
        -- \| The letter a forced join drags below the stack - a consonant or the
        -- a-chen, already subjoined or not - with the subjoining run under it. The
        -- forced letter leads and the run's letters trail in reverse, exactly where
        -- they print (g+mra -> གྨྲ); a caret that survived the run lands between them
        -- (f+ra -> ཕ༹ྲ).
        pSubjoinBelow :: Parser TibetanSyllable
        pSubjoinBelow = do
            next <- MP.choice [GP.pConsonant, GP.pSubConsonant]
            (subjoined, caret) <- GP.pSubjoinRun
            pure $
                Seq.singleton (Subfix, next)
                    <> caretMark caret
                    <> Seq.fromList [(Subfix, s) | s <- reverse subjoined]

-- | Where the base stands: a word-initial prefix letter over a consonant is a
-- Prefix (bsgribs -> བ), a superfix letter over a non-subfix consonant is a
-- Superfix (rka -> རྐ), anything else is the Root. The renderer prints the
-- letter either way; the positions matter to the legality tables of wave 3.4.
markBase :: Bool -> Token -> Maybe Token -> (Position, Token)
markBase atStart base next =
    case next of
        Just n
            -- The letter directly below the base belongs to its own stack
            -- (bra -> བྲ), so a subjoining letter is never a prefix.
            | atStart
                && GP.isPrefixLetter base
                && GP.isConsonantToken n
                && not (GP.isSubjoinCandidate n)
                && not (GP.isImplicitA n) ->
                (Prefix, base)
            | GP.isSuperfixLetter base
                && not (GP.isSubjoinCandidate n)
                && not (GP.isImplicitA n) ->
                (Superfix, base)
        _ -> (Root, base)
