-- | The ambiguous-syllable rule of wave 3.4.5: a syllable whose letters stand
-- the same way in two readings, where the corpus prefers one of them.
--
-- The rule is a pure function of the stacks a completed syllable carries, and
-- the window that runs it is hosted where the syllable ends - the tail of the
-- three structures that claim the ambiguous forms. It cannot live inside
-- 'Convert.Grammar.Constraint.Constraint14': that rule is also the first half
-- of the structures 9, 13 and 35, where the word is not finished yet, and its
-- marks there (dgas: prefix, root, implicit vowel) are the very marks this
-- rule must read as the two-letter form (dga). Only the completed syllable
-- says which reading won.
--
-- The recommendation speaks of Wylie, so the implicit @a@ must be the letter
-- @a@ the Wylie writer wrote, not just a fact of the stack: after 6b every
-- glyph-sourced root stack carries 'VowelImplicit' too, because the glyphs
-- write no inherent vowel, and a Tibetan syllable is no recommendation. The
-- source of the root sign says which of the two it is; a written vowel closes
-- the slot in both. That slot is therefore also what keeps the rule off a
-- syllable with a real vowel (dgi is དགི, not དག).
module Convert.Grammar.Constraint.Constraint22
    ( recommendedSpelling
    , noteAmbiguous
    ) where

import Control.Monad (guard)
import Convert.Diagnostic (Finding (PreferredSpelling))
import Convert.Grammar.Constraint.Constraint15 (suffixConsonants15)
import Convert.Grammar.Parser
    ( SpellParser
    , noteFinding
    , peekFindings
    , peekStacks
    )
import Convert.Grammar.Syllable
    ( Role (..)
    , Stack (..)
    , Subfixes (..)
    , VowelSlot (..)
    , finalsTokens
    )
import Convert.Token
    ( Consonant (..)
    , Token
    , TokenCanonical (TcConsonant)
    , TokenSource (TsWylie)
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import Data.Text (Text)

-- | Where the preferred reading of a three-letter form puts its root: the first
-- of the three letters, or the second.
data RootAt
    = RootFirst
    | RootSecond
    deriving (Show, Eq)

-- | The three-letter forms the corpus reads one way while the grammar reads
-- another: the three consonants in the order they stand, and the reading the
-- corpus prefers of them.
--
-- Five of the eight are the same reading with the letters shifted: dags,
-- dabs, dams, 'ags and 'abs stand prefix-free (root + suffix + postfix), and
-- the preferred reading puts the first letter back in front as a prefix -
-- dgas, dbas, dmas, 'gas, 'bas, root second. The other two run the other way:
-- bgas and mgas stand with the prefix, and bags, mags are the preferred
-- reading, root first.
--
-- Seven forms, each a fixed three-letter shape, and the rule below only ever
-- asks whether one is here - so the forms are read straight off the letters
-- rather than looked up in a table keyed by them.
ambiguousForm :: [Consonant] -> Maybe (RootAt, Text)
ambiguousForm [Cd, Cg, Cs] = Just (RootSecond, "dgas")
ambiguousForm [Cd, Cb, Cs] = Just (RootSecond, "dbas")
ambiguousForm [Cd, Cm, Cs] = Just (RootSecond, "dmas")
ambiguousForm [C', Cg, Cs] = Just (RootSecond, "'gas")
ambiguousForm [C', Cb, Cs] = Just (RootSecond, "'bas")
ambiguousForm [Cb, Cg, Cs] = Just (RootFirst, "bags")
ambiguousForm [Cm, Cg, Cs] = Just (RootFirst, "mags")
ambiguousForm _ = Nothing

-- | The spelling the corpus prefers of a completed syllable, or nothing when
-- the syllable reads the way it should.
--
-- The two-letter form reads prefix + root (dga), and the other reading of it -
-- root + suffix - is preferred whenever the root letter is a legal suffix
-- letter, which is the whole of the suffix group of rule 4.15 read from its
-- own data. The three-letter forms are listed in 'ambiguousForm', and the rule
-- speaks only where the parse put the root where that listing does not.
recommendedSpelling :: Seq Stack -> Maybe Text
recommendedSpelling stacks = case traverse plainStack (toList stacks) of
    Just [(RolePrefix, prefixLetter, False), (RoleRoot, rootLetter, True)] ->
        twoLetters prefixLetter rootLetter
    Just [(RolePrefix, c0, False), (RoleRoot, c1, True), (RoleSuffix, c2, False)] ->
        preferredByForm RootSecond [c0, c1, c2]
    Just [(RoleRoot, c0, True), (RoleSuffix, c1, False), (RolePostfix, c2, False)] ->
        preferredByForm RootFirst [c0, c1, c2]
    _ -> Nothing
    where
        -- The recommended reading of two consonants is the one with the root
        -- first, so the Wylie is the letters with the @a@ between them.
        twoLetters prefixLetter rootLetter =
            case consonantOf rootLetter of
                Just rootConsonant
                    | rootConsonant `elem` suffixConsonants15 ->
                        Just (tokenRaw prefixLetter <> "a" <> tokenRaw rootLetter)
                _ -> Nothing

        -- The form answers only where the parse disagrees with it: the reading
        -- it prefers is the recommendation, and a syllable already read that
        -- way is silent.
        preferredByForm rootAt letters = do
            consonants <- mapM consonantOf letters
            (preferred, spelling) <- ambiguousForm consonants
            guard (rootAt /= preferred)
            pure spelling

-- | A stack that is its base letter and nothing else, with or without the
-- implicit @a@ (the flag): the only shape the three readings above are written
-- for. A stack with a written vowel, a superfix, a subfix, a caret or a final
-- is a different reading and matches none of them.
plainStack :: Stack -> Maybe (Role, Token, Bool)
plainStack st = do
    root <- stRoot st
    guard $
        isNothing (stSuperfix st)
            && stSubfixes st == NoSubfix
            && isNothing (stCaret st)
            && null (finalsTokens (stFinals st))
    implicit <- case stVowel st of
        Nothing -> Just False
        -- The written letter @a@ is the only implicit vowel the rule speaks
        -- about: a glyph's 'VowelImplicit' is the same slot holding no letter.
        Just VowelImplicit -> Just (tokenSource root == TsWylie)
        Just VowelWritten{} -> Nothing
    pure (stRole st, root, implicit)

consonantOf :: Token -> Maybe Consonant
consonantOf token = case tokenCanonical token of
    TcConsonant consonant -> Just consonant
    _ -> Nothing

-- | The window: the syllable is complete, so the rule reads the stacks the
-- structure has just built and records what it finds.
--
-- The reference keeps quiet when the run carries a warning already, and so
-- does this: a run that another window has already blamed has no business
-- carrying a second, vaguer one. (The book structures host no other window
-- yet, so the gate holds by construction today and names the intent
-- tomorrow.)
noteAmbiguous :: SpellParser ()
noteAmbiguous = do
    others <- peekFindings
    stacks <- peekStacks
    case recommendedSpelling stacks of
        Just spelling
            | null others -> noteFinding (PreferredSpelling spelling)
        _ -> pure ()
