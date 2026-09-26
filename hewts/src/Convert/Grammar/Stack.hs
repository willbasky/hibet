-- | The generic word structure: a syllable-shaped stack the book's 37
-- structures do not spell.
--
-- The book rules cover the thirty roots, the four subfix letters and the five
-- Sanskrit letters; the references accept more stacks than the book lists -
-- bare-letter stacks such as @kya@ and @sgrwa@ (subfixes written in full
-- letters, which Wylie does), explicit stacks such as @sat+t+wa@ and @g.yon@,
-- a caret anywhere in the stack, a word-initial vowel. This structure fills
-- exactly that gap and is probed last: whatever none of the 37 book shapes
-- can match, the generic stack tries. Probe-last keeps the book in charge - a
-- word the book spells keeps the book's marks, a word it does not gets the
-- closest thing the references agree on.
--
-- The marks follow jsewts (convertors/main/jsewts), the reference whose
-- corpus makes up most of the parity vectors:
--
--   * the subjoining letters are @{y, w, r, l}@ - a bare letter in Wylie, an
--     already-joined subconsonant token in Tibetan - at most two per stack,
--     and @l@ never sits below two consonants (grla is ག + ར + ླ, not གྲླ);
--   * a stack whose subjoining run ends on a consonant instead of a vowel
--     keeps only its first consonant and the parse starts over from the
--     second (jsewts's own backtrack);
--   * @+@ forces the next letter into the stack (its subjoined form, or the
--     vowel sign); @.@ ends the stack (g.yon -> གཡོན);
--   * carets are transparent while subjoining, collapse to one, and print
--     between the subfixes and the vowel (gra^ stays at the very end);
--   * a word-initial vowel gets the a-chen written out in the renderer.
-- The legality tables (which roots admit which subfixes) come in wave 3.4;
-- this structure is deliberately as permissive as the references are.
module Convert.Grammar.Stack (pStack) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import Convert.Grammar.Word (Position (..), TibetanWord)
import Convert.Token
    ( ConSpec (CSDot, CSPlus)
    , Consonant (C', Ca, Cb, Cd, Cg, Cl, Cm, Cr, Cs, Cw, Cy)
    , FinalMark (FMCaret)
    , SubConsonant (SCl)
    , Token (..)
    , TokenCanonical (..)
    , TokenSource (..)
    , Vowel (Ve, Vi, Vo, Vu)
    )
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Sequence as Seq
import qualified Text.Megaparsec as MP

-- | Match a chunk shaped like a syllable: anything the 37 book structures do
-- not name, as long as it starts with a consonant or a vowel and holds only
-- stack letters. The 'Spelling' argument exists for form; the lattice is
-- decided per token, by 'tokenSource' (the parity run parses Wylie token
-- lists under 'Tibetan' anyway, so 'Spelling' cannot be trusted here).
pStack :: Spelling -> Parser TibetanWord
pStack _spelling = do
    t0 <- MP.choice [MP.satisfy isConsonantLike, pVowelAny]
    rest <- MP.many pStackToken
    pure (Seq.fromList (markWord (t0 : rest)))

-- | One token that may continue a stack. @+@ and @.@ only count when they do
-- something (the letter after them belongs to the stack), so they are probed
-- in lookahead and left alone otherwise.
pStackToken :: Parser Token
pStackToken =
    MP.choice
        [ MP.try (MP.satisfy isPlus <* MP.lookAhead stackLetter)
        , MP.try (MP.satisfy isDot <* MP.lookAhead (MP.satisfy isConsonantLike))
        , MP.satisfy isConsonantLike
        , MP.satisfy isSubConsonantLike
        , pVowelAny
        , MP.satisfy isFinalLike
        ]
    where
        stackLetter =
            MP.choice
                [ MP.satisfy isConsonantLike
                , MP.satisfy isSubConsonantLike
                , pVowelAny
                ]

-- | A vowel the stack can absorb. Wylie writes the long letters as their two
-- short parts (AH, mA, oM), so every vowel token is absorbable there; Tibetan
-- spells a long a explicitly (0x0f71) and a root written next to it must not
-- swallow it into the same word, so the Tibetan side takes only the short
-- vowels.
pVowelAny :: Parser Token
pVowelAny = MP.satisfy isEatableVowel

-- | Mark one chunk: walk it stack by stack, the way jsewts's
-- @fromWylieOneStack@ does. Every stack is the base letter plus the letters
-- subjoined to it plus its vowel; when a stack cannot reach a vowel it keeps
-- only its first consonant and everything after it begins a fresh stack.
markWord :: [Token] -> [(Position, Token)]
markWord = go True
    where
        go :: Bool -> [Token] -> [(Position, Token)]
        go _ [] = []
        go atStart ts =
            case takeStack atStart ts of
                (marks, rest) -> marks <> go False rest

        -- Consume one stack from the front. Some shapes are not a real stack at
        -- all - a lone final mark, a stack-breaking dot, a subjoined letter the
        -- previous stack could not keep - and each just passes through.
        takeStack :: Bool -> [Token] -> ([(Position, Token)], [Token])
        takeStack _ (t : rest)
            | isEatableVowel t = ([(Vowel, t)], rest)
            | isDot t = ([], rest)
            | isFinalLike t = ([(Final, t)], rest)
            | isSubConsonantLike t = ([(Subfix, t)], rest)
        takeStack atStart (t : rest) = markStack atStart t rest
        takeStack _ [] = ([], [])

        -- A stack starting on a consonant: choose the subfixes below it. If the
        -- stack reaches a vowel or a @+@, it is complete (base, subfixes, caret,
        -- vowel, forced joins, finals); if it ends on a consonant instead, it
        -- keeps only the base and the parse takes over from the second letter.
        markStack :: Bool -> Token -> [Token] -> ([(Position, Token)], [Token])
        markStack atStart base rest =
            let (subs, caret, after) = takeSubs rest
                next = firstNonCaret after
             in case next of
                    Just n
                        | isVowelLike n || isPlus n ->
                            let (tailMarks, tailRest) = consumeTail after
                             in ( markBase atStart base rest
                                    : [(Subfix, s) | s <- subs]
                                        <> [(Final, c) | c <- maybeToList caret]
                                        <> tailMarks
                                , tailRest
                                )
                    _ ->
                        -- no vowel in sight: the stack collapses to its first
                        -- consonant and the rest is re-parsed from scratch
                        ([(markBase atStart base rest)], rest)

        -- Choose the subfixes below a base: the letters @{y, w, r, l}@, at most
        -- two (with @l@ never second), and the carets in between, which are
        -- transparent while the scan goes on.
        takeSubs :: [Token] -> ([Token], Maybe Token, [Token])
        takeSubs = scan [] Nothing
            where
                scan :: [Token] -> Maybe Token -> [Token] -> ([Token], Maybe Token, [Token])
                scan subs caret toks =
                    case toks of
                        tok : more
                            | isCaretLike tok -> scan subs (firstCaret caret tok) more
                            | length subs < 2 && isSubjoinCandidate tok && not (length subs == 1 && isL tok) ->
                                scan (subs <> [tok]) caret more
                            | otherwise -> (subs, caret, toks)
                        [] -> (subs, caret, [])
                firstCaret Nothing tok = Just tok
                firstCaret kept _ = kept

        -- The rest of a complete stack: consecutive vowels (@a@ among them, which
        -- is the letter that never prints), forced subjoins (@+X@), a repeated
        -- subjoining run after a forced one (g+mra), a caret and then finals. A
        -- dot ends the tail here; the next 'takeStack' consumes it.
        consumeTail :: [Token] -> ([(Position, Token)], [Token])
        consumeTail = goTail False []
            where
                -- After a real vowel the tail is over: a bare @a@ that follows (goang
                -- གོཨང) is the root ཨ of a fresh stack, not the implicit vowel again.
                goTail ::
                    Bool -> [(Position, Token)] -> [Token] -> ([(Position, Token)], [Token])
                goTail vowelSeen acc toks =
                    case toks of
                        tok : more
                            | isEatableVowel tok -> goTail True ((Vowel, tok) : acc) more
                            | isCa tok ->
                                if vowelSeen
                                    then (reverse acc, toks)
                                    else goTail False ((ImplicitVowel, tok) : acc) more
                            | isCaretLike tok -> goTail vowelSeen ((Final, tok) : acc) more
                            | isFinalLike tok -> goTail vowelSeen ((Final, tok) : acc) more
                            | isPlus tok ->
                                case more of
                                    next : rest
                                        | isConsonantLike next || isSubConsonantLike next ->
                                            let (subs, caret, after) = takeSubs rest
                                             in goTail
                                                    vowelSeen
                                                    ( [(Subfix, s) | s <- subs]
                                                        <> [(Final, c) | c <- maybeToList caret]
                                                        <> [(Subfix, next)]
                                                        <> acc
                                                    )
                                                    after
                                        | isEatableVowel next -> goTail True ((Vowel, next) : acc) rest
                                        | isCa next -> goTail vowelSeen ((Subfix, next) : acc) rest
                                        | otherwise -> (reverse acc, toks)
                                    [] -> (reverse acc, toks)
                            | otherwise -> (reverse acc, toks)
                        [] -> (reverse acc, [])

        -- Where the base stands: a word-initial prefix letter over a consonant is
        -- a Prefix (bsgribs -> བ), a superfix letter over a non-subfix consonant
        -- is a Superfix (rka -> རྐ), anything else is the Root. The renderer
        -- prints the letter either way; the positions matter to the legality
        -- tables of wave 3.4.
        markBase :: Bool -> Token -> [Token] -> (Position, Token)
        markBase atStart base rest =
            case firstNonCaret rest of
                Just next
                    -- The letter directly below the base belongs to its own
                    -- stack (bra -> བྲ), so a subjoining letter is never a
                    -- prefix; the prefix-letter tests ride on this.
                    | atStart
                        && isPrefixLetter base
                        && isConsonantLike next
                        && not (isSubjoinCandidate next)
                        && not (isCa next) ->
                        (Prefix, base)
                    | isSuperfixLetter base && not (isSubjoinCandidate next) && not (isCa next) ->
                        (Superfix, base)
                _ -> (Root, base)

        firstNonCaret :: [Token] -> Maybe Token
        firstNonCaret = listToMaybe . dropWhile isCaretLike

-- | Whether a vowel token belongs to a stack (see 'pVowelAny').
isEatableVowel :: Token -> Bool
isEatableVowel tok@Token{tokenCanonical = TcVowel v}
    | tokenSource tok == TsUnicode = v `elem` [Vi, Ve, Vo, Vu]
    | otherwise = True
isEatableVowel _ = False

-- | The letter @a@ where a vowel would be: written in every Wylie syllable
-- and never printed.
isCa :: Token -> Bool
isCa Token{tokenCanonical = TcConsonant Ca} = True
isCa _ = False

isVowelLike :: Token -> Bool
isVowelLike tok = isEatableVowel tok || isCa tok

isCaretLike :: Token -> Bool
isCaretLike Token{tokenCanonical = TcFinal FMCaret} = True
isCaretLike _ = False

isFinalLike :: Token -> Bool
isFinalLike Token{tokenCanonical = TcFinal _} = True
isFinalLike _ = False

isConsonantLike :: Token -> Bool
isConsonantLike Token{tokenCanonical = TcConsonant _} = True
isConsonantLike _ = False

isSubConsonantLike :: Token -> Bool
isSubConsonantLike Token{tokenCanonical = TcSubConsonant _} = True
isSubConsonantLike _ = False

isPlus :: Token -> Bool
isPlus Token{tokenCanonical = TcConSpec CSPlus} = True
isPlus _ = False

isDot :: Token -> Bool
isDot Token{tokenCanonical = TcConSpec CSDot} = True
isDot _ = False

isL :: Token -> Bool
isL Token{tokenCanonical = TcConsonant Cl} = True
isL Token{tokenCanonical = TcSubConsonant SCl} = True
isL _ = False

isPrefixLetter :: Token -> Bool
isPrefixLetter Token{tokenCanonical = TcConsonant c} = c `elem` [C', Cb, Cd, Cg, Cm]
isPrefixLetter _ = False

isSuperfixLetter :: Token -> Bool
isSuperfixLetter Token{tokenCanonical = TcConsonant c} = c `elem` [Cl, Cr, Cs]
isSuperfixLetter _ = False

-- | A letter that can sit below another one: the bare @{y, w, r, l}@ a Wylie
-- writer spells in full letters, or a subconsonant token Tibetan spells
-- already joined. A Tibetan bare letter is a real letter and never a subjoin.
isSubjoinCandidate :: Token -> Bool
isSubjoinCandidate tok@Token{tokenCanonical = TcConsonant c}
    | tokenSource tok == TsWylie = c `elem` [Cl, Cr, Cw, Cy]
    | otherwise = False
isSubjoinCandidate Token{tokenCanonical = TcSubConsonant _} = True
isSubjoinCandidate _ = False
