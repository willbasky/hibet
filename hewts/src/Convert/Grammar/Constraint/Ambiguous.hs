-- | The ambiguous-syllable rule of wave 3.4.5: a syllable whose letters stand
-- the same way in two readings, where the corpus prefers one of them.
--
-- The rule is a pure function of the marks a completed syllable carries, and
-- the window that runs it is hosted where the syllable ends - the tail of the
-- three structures that claim the ambiguous forms. It cannot live inside
-- 'Convert.Grammar.Constraint.Constraint14': that rule is also the first half
-- of the structures 9, 13 and 35, where the word is not finished yet, and its
-- marks there (dgas: prefix, root, implicit vowel) are the very marks this
-- rule must read as the two-letter form (dga). Only the completed syllable
-- says which reading won.
--
-- The recommendation speaks of Wylie, so the input must be Wylie too: a
-- syllable carries the 'ImplicitVowel' mark exactly when the letter @a@ was
-- written where a vowel would be, and the Tibetan spelling never writes it
-- (དག is read root ད + suffix ག, with no letter standing in for a vowel).
-- That mark is therefore also what keeps the rule off a syllable with a real
-- vowel (dgi is དགི, not དག).
module Convert.Grammar.Constraint.Ambiguous
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
    )
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token
    ( Consonant (..)
    , Token
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    , tokenRaw
    )
import Data.Foldable (toList)
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
recommendedSpelling :: TibetanSyllable -> Maybe Text
recommendedSpelling syllable
    | not (any isImplicitVowelMark marked) = Nothing
    | otherwise = case marked of
        [(Prefix, prefixLetter), (Root, rootLetter), (ImplicitVowel, _)] ->
            twoLetters prefixLetter rootLetter
        [(Prefix, c0), (Root, c1), (ImplicitVowel, _), (Suffix, c2)] ->
            preferredByForm RootSecond [c0, c1, c2]
        [(Root, c0), (ImplicitVowel, _), (Suffix, c1), (Postfix, c2)] ->
            preferredByForm RootFirst [c0, c1, c2]
        _ -> Nothing
    where
        marked = toList syllable

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

isImplicitVowelMark :: (Position, Token) -> Bool
isImplicitVowelMark (ImplicitVowel, _) = True
isImplicitVowelMark _ = False

consonantOf :: Token -> Maybe Consonant
consonantOf token = case tokenCanonical token of
    TcConsonant consonant -> Just consonant
    _ -> Nothing

-- | The window: the syllable is complete, so the rule reads the marks the
-- structures have just placed and records what it finds.
--
-- The reference keeps quiet when the run carries a warning already, and so
-- does this: a run that another window has already blamed has no business
-- carrying a second, vaguer one. (The book structures host no other window
-- yet, so the gate holds by construction today and names the intent
-- tomorrow.)
noteAmbiguous :: TibetanSyllable -> SpellParser ()
noteAmbiguous syllable = do
    others <- peekFindings
    case recommendedSpelling syllable of
        Just spelling
            | null others -> noteFinding (PreferredSpelling spelling)
        _ -> pure ()
