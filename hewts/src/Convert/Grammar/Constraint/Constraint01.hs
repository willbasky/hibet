{- Tibetan spelling grammar 4.1 (token parser variant) -}

module Convert.Grammar.Constraint.Constraint01
    ( pConstraint01
    , pConstraint01WithLong
    , pConstraint01Sanskrit
    ) where

import Convert.Grammar.Parser
    ( SpellParser
    , markRoot
    , markVowel
    , vowelSlot
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import qualified Data.Sequence as Seq

-- | The root a stack already spelled: the a-chen standing there is no
-- consonant waiting for its vowel - it already is that vowel, and a stack
-- carries a single vowel. So when the base is this letter the Wylie vowel
-- slot stays empty whatever comes - a second @a@, a short sign or a long one
-- (@aa@, @ae@ and @aA@ are two stacks each, as the reference reads them).
-- The run stays for the word parser, which reads the next vowel as the root
-- of its own stack.
rootIsA :: TibetanSyllable -> Bool
rootIsA = maybe False GP.isImplicitA . Seq.lookup 0

-- | The root, then its vowel slot. The Wylie a-chen root takes no vowel.
rooted ::
    SpellParser TibetanSyllable
    -> [SpellParser TibetanSyllable]
    -> SpellParser TibetanSyllable
rooted root vowels = do
    r <- root
    v <-
        if rootIsA r && GP.isWylieStack r
            then pure mempty
            else vowelSlot r vowels
    pure (r <> v)

pConstraint01 :: SpellParser TibetanSyllable
pConstraint01 = rooted (markRoot GP.pRootConsonant) [markVowel GP.pVowel]

pConstraint01WithLong :: SpellParser TibetanSyllable
pConstraint01WithLong =
    rooted
        (markRoot GP.pRootConsonant)
        [markVowel GP.pVowel, markVowel GP.pVowelLongA]

pConstraint01Sanskrit :: SpellParser TibetanSyllable
pConstraint01Sanskrit = rooted (markRoot GP.pSanskrit) [markVowel GP.pVowel]
