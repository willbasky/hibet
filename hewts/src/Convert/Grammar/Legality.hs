-- | Spelling legality: whether the letters of a syllable may stand where they
-- stand. The tokenizers decode the input and the grammar (wave 3) marks each
-- token with the place it occupies, but neither checks that the arrangement is
-- legal - ཀྒ and གྷ both come out marked fine. This layer adds the reference's
-- warnings for each syllable: subjoined letters the root cannot take, prefixes
-- that cannot lead the letters that follow, two bindu in one stack, a suffix
-- that may not close the syllable, and the "syllable should probably be"
-- hints.
--
-- The unit of analysis is the *syllable run* up to the next boundary, in
-- source order (wave 3.4 makes the boundary part of the syllable item;
-- 'Convert.Sentence' hands the checker the run's tokens without the trailing
-- boundary, so the quoted span still covers exactly what the reference
-- blames). A run that fell apart under the grammar still reaches the checker
-- as one piece, because the reference blames the whole run (the "Invalid
-- prefix consonant" for @tgra@ names the whole run, not the piece we parsed).
-- A stack dot, a caret or a plus stays inside its run, so the checker sees the
-- whole run even though the grammar eats those separators without marking
-- them.
--
-- The checker is a pure function of the token list: it decides from the tokens
-- alone, never from the marks or the parse. Wave 3.4.1 ships the machinery
-- with the tokenizer diagnostics; 3.4.2 adds the prefix position; the rules
-- arrive one wave sub-step at a time into 'checkWord'.
module Convert.Grammar.Legality
    ( checkWord
    ) where

import Convert.Diagnostic
    ( Diagnostics
    , addDiagnostic
    , invalidPrefixConsonant
    , prefixNotBefore
    )
import Convert.Token
    ( ConSpec (..)
    , Consonant (..)
    , Span (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , offsetEnd
    , offsetStart
    , tokenCanonical
    , tokenRaw
    , tokenSource
    , tokenSpan
    )
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | The reference's warnings for one syllable run: the tokens up to the next
-- boundary, in source order. All of 3.4's rules land here, one sub-step at a
-- time; 3.4.2 adds the prefix position: how the reference reads a run's first
-- stack, and the two prefix warnings (section 4.2, the reference's
-- @m_prefixes@).
checkWord :: [Token] -> Diagnostics
checkWord word = case prefixReading word of
    PrefixSingle cons quote next ->
        let sp = wordSpan word
         in case M.lookup cons prefixAfter of
                Just allowed ->
                    case next of
                        Just n -> case tokenConsonant n of
                            -- allowed consonant: stay silent; everything else
                            -- (a consonant outside the table, a stack dot, an
                            -- unknown letter) is exactly what the reference
                            -- blames ("g....yag" names the dot, "tgra" the
                            -- whole run)
                            Just c
                                | c `Set.member` allowed -> mempty
                            _ ->
                                addDiagnostic
                                    (prefixNotBefore sp (Just sp) quote (T.filter (/= '+') (tokenRaw n)))
                                    mempty
                        _ -> mempty
                Nothing ->
                    addDiagnostic (invalidPrefixConsonant sp (Just sp) quote) mempty
    _ -> mempty

-- | The consonant a token spells, or 'Nothing' for a non-consonant: a
-- stack-break dot, a vowel, a mark, an unknown letter.
tokenConsonant :: Token -> Maybe Consonant
tokenConsonant t = case tokenCanonical t of
    TcConsonant c -> Just c
    _ -> Nothing

-- | The prefix tables of section 4.2 (the reference's @m_prefixes@): the
-- letters that may lead a syllable, and the letters each may lead. The
-- non-strict comparison names the one token right behind the head consonant,
-- so an explicit stack (written with '+') never hits until the strict
-- sub-steps.
prefixAfter :: M.Map Consonant (Set.Set Consonant)
prefixAfter =
    M.fromList
        [ (Cg, Set.fromList [Cc, Cny, Ct, Cd, Cn, Cts, Czh, Cz, Cy, Csh, Cs])
        , (Cd, Set.fromList [Ck, Cg, Cng, Cp, Cb, Cm])
        , (Cb, Set.fromList [Ck, Cg, Cc, Ct, Cd, Cts, Czh, Cz, Csh, Cs, Cr, Cl])
        , (Cm, Set.fromList [Ckh, Cg, Cng, Cch, Cj, Cny, Cth, Cd, Cn, Ctsh, Cdz])
        , (C', Set.fromList [Ckh, Cg, Cch, Cj, Cth, Cd, Cph, Cb, Ctsh, Cdz])
        ]

-- | The superfix tables of wave 3.3 (the reference's @m_superscripts@): the
-- letters above which each of @r@, @l@ and @s@ may sit. The checker uses them
-- as the gate that decides whether a run opening with r, l or s holds a
-- superfix stack rather than a prefix position.
superfixAfter :: M.Map Consonant (Set.Set Consonant)
superfixAfter =
    M.fromList
        [ (Cr, Set.fromList [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cb, Cm, Cts, Cdz])
        , (Cl, Set.fromList [Ck, Cg, Cng, Cc, Cj, Ct, Cd, Cp, Cb, Ch])
        , (Cs, Set.fromList [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cp, Cb, Cm, Cts])
        ]

-- | How the reference's stack scanner reads a run's head, reduced to what the
-- prefix check needs.
data PrefixReading
    = -- | No consonant at the head: a vowel, the a-chen, a mark.
      PrefixNone
    | -- | The first stack is not a bare consonant (a vowel, a final, an explicit
      -- plus or a superfix root sits in it): nothing holds the PREFIX state.
      PrefixStack
    | -- | A bare consonant in the PREFIX state: its consonant, the source slice
      -- it is quoted by, and the \"next\" token the reference quotes ('Nothing'
      -- at the run's end).
      PrefixSingle Consonant Text (Maybe Token)
    deriving (Show)

-- | Read the head of a run the way the reference reads the first stack of a
-- syllable, stopping at what the prefix check needs: whether a bare consonant
-- sits in its PREFIX state, and which token it would blame behind it. Mirrors
-- jsewts 'fromWylieOneStack' and its PREFIX branch: a superfix letter gates its
-- root into the stack, subscripts join the stack, and a stack that never
-- reaches a vowel falls back to the head consonant alone (the reference
-- backtracks).
prefixReading :: [Token] -> PrefixReading
prefixReading word = case word of
    [] -> PrefixNone
    t0 : rest
        -- the prefix position is a W->U notion; the back-conversion warns on
        -- its own channel, so a Unicode run never sits here
        | tokenSource t0 /= TsWylie -> PrefixNone
        | otherwise -> case tokenCanonical t0 of
            TcConsonant c
                -- the written @a@ is the a-chen, never a prefix consonant
                | c /= Ca
                , not (T.null (tokenRaw t0)) ->
                    readStack c (tokenRaw t0) (dropCompoundTail rest)
            _ -> PrefixNone
    where
        -- The first stack: an explicit plus makes it non-single, a superfix letter
        -- may gate its root in, and then the stack body decides.
        readStack cons quote rest =
            if T.isInfixOf "+" quote
                then PrefixStack
                else case superfixRoot cons rest of
                    Just afterRoot -> readBody cons quote rest (2, dropCompoundTail afterRoot)
                    Nothing -> readBody cons quote rest (1, rest)

        readBody :: Consonant -> Text -> [Token] -> (Int, [Token]) -> PrefixReading
        readBody cons quote followed (count, toks) =
            let (nSubscripts, after) = consumeSubscripts count toks
             in case listToMaybe after of
                    Just t
                        | isStackVowel t -> PrefixStack
                        | isStackFinal t -> PrefixStack
                        | isExplicitPlus t -> PrefixStack
                    _
                        | count + nSubscripts > 1 ->
                            -- no vowel: the stack backtracks to the bare head and
                            -- quotes the token right behind it
                            PrefixSingle cons quote (firstWylie followed)
                        | otherwise ->
                            -- a single-consonant stack silently eats one dot
                            PrefixSingle cons quote (skipOneDot after)

        -- A superfix letter opens a superfix stack when the letter right behind it
        -- is one its table allows (so rka, sgra, lta hold a stack, never a prefix).
        superfixRoot cons rest =
            case (M.lookup cons superfixAfter, rest) of
                (Just allowed, t1 : more) -> case tokenConsonant t1 of
                    Just c
                        | c `Set.member` allowed -> Just more
                    _ -> Nothing
                _ -> Nothing

        -- The subconsonant tokens a compound head leaves behind (བྷ = b + ྷ) sit in
        -- its own slice and join the stack without counting as letters.
        dropCompoundTail = dropWhile isSubConsonant

        isSubConsonant t = case tokenCanonical t of
            TcSubConsonant _ -> True
            _ -> False

        -- At most two subscripts join a stack, and lata never sits below more than
        -- one consonant (that keeps @brla@ = b.r+la together).
        consumeSubscripts count0 rest = go 0 rest
            where
                go n toks
                    | n >= 2 = (n, toks)
                    | otherwise = case toks of
                        t : more
                            | isSubscriptLetter t ->
                                if isLata t && count0 + n > 1
                                    then (n, toks)
                                    else
                                        let (n', tail') = go (n + 1) more
                                         in (n', tail')
                        _ -> (n, toks)

        isSubscriptLetter t = case tokenCanonical t of
            TcConsonant Cy -> True
            TcConsonant Cr -> True
            TcConsonant Cl -> True
            TcConsonant Cw -> True
            _ -> False

        isLata t = tokenCanonical t == TcConsonant Cl

        -- The first token with a source slice, or 'Nothing' at the run's end.
        firstWylie toks = case toks of
            t : _ | not (T.null (tokenRaw t)) -> Just t
            _ -> Nothing

        -- A single-consonant stack silently eats one stack-break dot, so the
        -- \"next\" token is the one beyond it (g.yag names y, g....yag names .).
        skipOneDot toks = case toks of
            t : more | isStackDot t -> firstWylie more
            _ -> firstWylie toks

        isStackVowel t = case tokenCanonical t of
            TcVowel _ -> True
            TcConsonant Ca -> True
            _ -> False

        isStackFinal t = case tokenCanonical t of
            TcFinal _ -> True
            _ -> False

        isExplicitPlus t = tokenCanonical t == TcConSpec CSPlus

        isStackDot t = tokenCanonical t == TcConSpec CSDot

-- | The span of a whole syllable run, from its first to its last token.
wordSpan :: [Token] -> Span
wordSpan word@(first : _) =
    Span
        (offsetStart (tokenSpan first))
        (offsetEnd (tokenSpan (foldl (\_ t -> t) first word)))
wordSpan [] = Span 0 0
