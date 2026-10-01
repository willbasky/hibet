-- | Non-fatal diagnostics: what the converter noticed while reading input.
--
-- The wording, the codes and the composition of the channel are our own (wave
-- 3.4, 30.09.2026): the reference comparisons cover conversion only, never the
-- warnings.
--
-- A 'Diagnostic' carries *positions*, not text: the 'Span' where the problem was
-- found, and optionally the 'Span' of the word to blame. Line numbers and the
-- quoted word are resolved once, at the edge, from the input itself - the same
-- place the reference builds its messages, and the reason no position is ever
-- recomputed inside the tokenizers.
module Convert.Diagnostic
    ( Severity (..)
    , DiagnosticCode (..)
    , Diagnostic (..)
    , Diagnostics
    , addDiagnostic
    , diagnosticsInOrder
    , diagnosticList
    , unexpectedCharacter
    , unfinishedComment
    , invalidHexCode
    , orphanDot
    , leadingFinal
    , invalidPrefix
    , prefixCannotLead
    , repeatedCaret
    , duplicateFinal
    , forcedJoinAfterVowel
    , badSuperfixCombination
    , missingVowelAfterPrefix
    , invalidSecondSuffix
    , consonantAfterSecondSuffix
    , ambiguousSpelling
    , Finding (..)
    , resolveFinding
    , findingsDiagnostics
    , renderDiagnostic
    , renderDiagnostics
    ) where

import Convert.Token (Span, offsetEnd, offsetStart)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

data Severity
    = SevWarning
    | SevError
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The kind of problem a 'Diagnostic' reports. The catalogue of codes lives
-- here, in one place: the UI can map a code to its help text, and a converter
-- stops inventing parallel channels for the same finding. Spelling checks
-- (Wave 3.4) add their codes here as the rules land.
data DiagnosticCode
    = UnexpectedCharacter
    | UnfinishedComment
    | InvalidHexCode
    | OrphanDot
    | LeadingFinal
    | InvalidPrefix
    | PrefixCannotLead
    | RepeatedCaret
    | DuplicateFinal
    | ForcedJoinAfterVowel
    | IllegalSuperfixCombination
    | MissingVowelAfterPrefix
    | InvalidSecondSuffix
    | ConsonantAfterSecondSuffix
    | AmbiguousSpelling
    deriving (Show, Eq, Ord, Enum, Bounded)

data Diagnostic = Diagnostic
    { diagCode :: !DiagnosticCode
    , diagSeverity :: !Severity
    , diagSpan :: !Span
    , diagWord :: !(Maybe Span)
    , diagMessage :: !Text
    }
    deriving (Show, Eq)

-- | Accumulated diagnostics, held in *reverse* source order while the
-- converter runs so that 'addDiagnostic' stays O(1); 'diagnosticsInOrder'
-- restores source order. With that invariant the 'Monoid' is the obvious one:
-- combining two results concatenates them in source order (hence @b ++ a@).
newtype Diagnostics = Diagnostics [Diagnostic]
    deriving (Show, Eq)

instance Semigroup Diagnostics where
    Diagnostics a <> Diagnostics b = Diagnostics (b ++ a)

instance Monoid Diagnostics where
    mempty = Diagnostics []

-- | Record one diagnostic while walking the input.
addDiagnostic :: Diagnostic -> Diagnostics -> Diagnostics
addDiagnostic d (Diagnostics ds) = Diagnostics (d : ds)

-- | The same diagnostics in source order. Applying it twice changes nothing,
-- and afterwards the value composes with '<>' the obvious way.
diagnosticsInOrder :: Diagnostics -> Diagnostics
diagnosticsInOrder (Diagnostics ds) = Diagnostics (reverse ds)

-- | The diagnostics as a list, in the order the value carries them.
diagnosticList :: Diagnostics -> [Diagnostic]
diagnosticList (Diagnostics ds) = ds

-- | A letter or a sign that stands where no Wylie spelling expects one: the
-- character is echoed as it was written, and this says why it was not read.
unexpectedCharacter :: Span -> Char -> Diagnostic
unexpectedCharacter sp c =
    Diagnostic
        UnexpectedCharacter
        SevWarning
        sp
        Nothing
        ("The character \"" <> T.singleton c <> "\" belongs to no Wylie spelling.")

-- | A bracketed block of foreign text that is never closed: the block is
-- echoed as it stands, up to the end of the line.
unfinishedComment :: Span -> Diagnostic
unfinishedComment sp =
    Diagnostic
        UnfinishedComment
        SevWarning
        sp
        Nothing
        "The bracketed foreign text is never closed."

-- | A \\uXXXX escape whose code is not a valid hexadecimal number. Such an
-- escape is dropped, and the message quotes it exactly as it was written.
invalidHexCode :: Span -> Text -> Diagnostic
invalidHexCode sp raw =
    Diagnostic
        InvalidHexCode
        SevWarning
        sp
        Nothing
        ("The escape \"" <> raw <> "\" is not a valid code point.")

-- | A stack dot standing in a run that no structure used it in (ka.): a dot
-- only joins a letter to the next one, so here it joins nothing. The run is
-- quoted whole, as every spelling rule quotes it.
orphanDot :: Span -> Maybe Span -> Diagnostic
orphanDot sp word =
    Diagnostic
        OrphanDot
        SevWarning
        sp
        word
        "The stack dot \".\" joins no stack to a letter."

-- | A final mark with no letter in front of it (Mi, ???): a final hangs over a
-- letter, so on its own it closes nothing. The mark is a run of its own, and
-- the run is quoted whole.
leadingFinal :: Span -> Maybe Span -> Text -> Diagnostic
leadingFinal sp word mark =
    Diagnostic
        LeadingFinal
        SevWarning
        sp
        word
        ("The final \"" <> mark <> "\" closes no letter.")

-- | A run opens with a consonant in the reference's PREFIX state that is no
-- prefix letter at all, so nothing it leads can be legal. The word is quoted
-- whole.
invalidPrefix :: Span -> Maybe Span -> Text -> Diagnostic
invalidPrefix sp word letter =
    Diagnostic
        InvalidPrefix
        SevWarning
        sp
        word
        ("The letter \"" <> letter <> "\" cannot be a prefix.")

-- | A prefix letter leads a letter its table (section 4.2) does not allow.
prefixCannotLead :: Span -> Maybe Span -> Text -> Text -> Diagnostic
prefixCannotLead sp word prefix next =
    Diagnostic
        PrefixCannotLead
        SevWarning
        sp
        word
        ("The prefix \"" <> prefix <> "\" does not allow \"" <> next <> "\" after it.")

-- | A second caret in the same subjoining run: only the first prints below
-- the run (g^r^a), and a second one is exactly what the rule of the repeated
-- caret names.
repeatedCaret :: Span -> Maybe Span -> Diagnostic
repeatedCaret sp word =
    Diagnostic
        RepeatedCaret
        SevWarning
        sp
        word
        "The caret \"^\" occurs more than once in this stack."

-- | Two finals of the same orthographic class in one stack's tail (kaMM):
-- the classes of the nine final marks group the variants that never repeat,
-- so the duplicate window is decided by class, not by mark.
duplicateFinal :: Span -> Maybe Span -> Text -> Diagnostic
duplicateFinal sp word cls =
    Diagnostic
        DuplicateFinal
        SevWarning
        sp
        word
        ("Two finals of the \"" <> cls <> "\" class in one stack.")

-- | A forced join @+@ drags a consonant below a stack whose vowel is already
-- placed (ku+k): the join after the stack's own vowel should bring a vowel,
-- not a consonant.
forcedJoinAfterVowel :: Span -> Maybe Span -> Text -> Diagnostic
forcedJoinAfterVowel sp word letter =
    Diagnostic
        ForcedJoinAfterVowel
        SevWarning
        sp
        word
        ( "The join \"+\" places \""
            <> letter
            <> "\" below a stack that already has its vowel."
        )

-- | A superfix letter gates a root with subjoined letters that its tables
-- (4.8, 5.1) do not name (rkwa, lkya, rpa): the rule of the superfix
-- combination.
badSuperfixCombination ::
    Span -> Maybe Span -> Text -> Text -> [Text] -> Diagnostic
badSuperfixCombination sp word sf root subs =
    Diagnostic
        IllegalSuperfixCombination
        SevWarning
        sp
        word
        ( "The superfix \""
            <> sf
            <> "\" does not occur above \""
            <> root
            <> "\""
            <> case subs of
                [] -> "."
                _ -> " with \"" <> T.concat subs <> "\" below it."
        )

-- | A prefix opens a word whose root stack never reaches a vowel (bk): the
-- rule of the vowel after the prefix.
missingVowelAfterPrefix :: Span -> Maybe Span -> Text -> Diagnostic
missingVowelAfterPrefix sp word pre =
    Diagnostic
        MissingVowelAfterPrefix
        SevWarning
        sp
        word
        ("The stack the prefix \"" <> pre <> "\" leads carries no vowel.")

-- | The second suffix slot: a consonant that is no 2nd-suffix letter at all
-- (thabg), or one of the postfix letters over a first suffix it does not
-- pair with (kabd).
invalidSecondSuffix :: Span -> Maybe Span -> Text -> Maybe Text -> Diagnostic
invalidSecondSuffix sp word c2 first =
    Diagnostic
        InvalidSecondSuffix
        SevWarning
        sp
        word
        ( case first of
            Just c1 -> "The second suffix \"" <> c2 <> "\" does not occur after \"" <> c1 <> "\"."
            Nothing -> "The consonant \"" <> c2 <> "\" cannot be a second suffix."
        )

-- | A consonant after a legal second suffix (dagsg): nothing may follow the
-- word's last slot.
consonantAfterSecondSuffix :: Span -> Maybe Span -> Text -> Diagnostic
consonantAfterSecondSuffix sp word letter =
    Diagnostic
        ConsonantAfterSecondSuffix
        SevWarning
        sp
        word
        ("The consonant \"" <> letter <> "\" cannot follow a second suffix.")

-- | A syllable whose letters stand the same way in two readings, one of which
-- the corpus prefers (dga reads as prefix ད + root ག, while དག is root ད +
-- suffix ག; dags against dgas): the preferred spelling the recommendation
-- quotes. The syllable itself is not wrong, so the word is quoted whole and
-- the severity stays a warning.
ambiguousSpelling :: Span -> Maybe Span -> Text -> Diagnostic
ambiguousSpelling sp word preferred =
    Diagnostic
        AmbiguousSpelling
        SevWarning
        sp
        word
        ( "The syllable is ambiguous; the preferred spelling is \""
            <> preferred
            <> "\"."
        )

-- | A finding a constraint window of the grammar records before the run is
-- over: the run's whole span is only known once the structures have claimed
-- it, so the window records the pieces and 'Convert.Sentence' resolves them
-- into finished 'Diagnostic's with the run's span, through 'resolveFinding'.
-- The sum grows one constructor per wave-3.4 rule as the windows land; the
-- words are quoted whole, as the reference quotes @tgra@.
data Finding
    = -- | The head letter is no prefix letter at all.
      HeadNotAPrefix !Text
    | -- | A prefix letter leads a letter its table does not allow; both the
      -- prefix and the blamed letter.
      HeadPrefixCannotLead !Text !Text
    | -- | The second caret of a subjoining run (g^r^a): only the first one
      -- prints.
      SecondCaret
    | -- | Two finals of the same orthographic class in one stack (kaMM).
      DuplicateFinalClass !Text
    | -- | A forced join's consonant under a stack whose vowel is already
      -- placed (ku+k).
      JoinAfterVowel !Text
    | -- | A superfix letter over a root and subjoined letters outside its
      -- tables; the blamed root and the subjoined letters it carried.
      BadSuperfixCombination !Text !Text ![Text]
    | -- | A prefix whose root stack carried no vowel (bk).
      NoVowelAfterPrefix !Text
    | -- | The second suffix slot: a consonant that is no 2nd-suffix letter,
      -- or one that does not pair with the first suffix before it.
      BadSecondSuffix !Text !(Maybe Text)
    | -- | A consonant after a legal second suffix (dagsg).
      ConsonantAfter2ndSuffix !Text
    | -- | A syllable whose letters read either way, and the spelling the
      -- corpus prefers of it.
      PreferredSpelling !Text
    | -- | A stack dot in the unclaimed tail of a run (ka.): the dot is no
      -- part of any structure of the run, so it joins nothing. Recorded by
      -- the run parser itself, not by a constraint window - no window owns
      -- the tail.
      UnplacedDot
    | -- | A final mark standing as a run of its own (Mi): it closes no
      -- letter. The mark as it was written.
      FinalWithoutLetter !Text
    deriving (Show, Eq)

-- | A 'Finding' with the span of the whole syllable run it names.
resolveFinding :: Span -> Finding -> Diagnostic
resolveFinding sp (HeadNotAPrefix letter) = invalidPrefix sp (Just sp) letter
resolveFinding sp (HeadPrefixCannotLead prefix' next) = prefixCannotLead sp (Just sp) prefix' next
resolveFinding sp SecondCaret = repeatedCaret sp (Just sp)
resolveFinding sp (DuplicateFinalClass cls) = duplicateFinal sp (Just sp) cls
resolveFinding sp (JoinAfterVowel letter) = forcedJoinAfterVowel sp (Just sp) letter
resolveFinding sp (BadSuperfixCombination sf root subs) = badSuperfixCombination sp (Just sp) sf root subs
resolveFinding sp (NoVowelAfterPrefix pre) = missingVowelAfterPrefix sp (Just sp) pre
resolveFinding sp (BadSecondSuffix c2 first) = invalidSecondSuffix sp (Just sp) c2 first
resolveFinding sp (ConsonantAfter2ndSuffix c) = consonantAfterSecondSuffix sp (Just sp) c
resolveFinding sp (PreferredSpelling preferred) = ambiguousSpelling sp (Just sp) preferred
resolveFinding sp UnplacedDot = orphanDot sp (Just sp)
resolveFinding sp (FinalWithoutLetter mark) = leadingFinal sp (Just sp) mark

-- | The finished diagnostics of a run's findings, in the order the windows
-- recorded them ('foldr' + 'addDiagnostic', which prepends, keeps that
-- order).
findingsDiagnostics :: Span -> [Finding] -> Diagnostics
findingsDiagnostics sp = foldr (addDiagnostic . resolveFinding sp) mempty

-- | One message in the converter's channel format: @line N: "word": message@,
-- where the word is quoted only when the diagnostic blames a specific word.
renderDiagnostic :: Text -> Diagnostic -> Text
renderDiagnostic input d =
    "line " <> T.pack (show (lineOf offset)) <> ": " <> body
    where
        offset = toInt (offsetStart (diagSpan d))
        body = case diagWord d of
            Nothing -> diagMessage d
            Just sp -> "\"" <> slice (offsetStart sp) (offsetEnd sp) <> "\": " <> diagMessage d
        slice from to = T.take len (T.drop (toInt from) input)
            where
                len = toInt to - toInt from
        lineOf off = length (takeWhile (<= off) (lineStarts input))

-- | All messages, ordered by position in the input.
renderDiagnostics :: Text -> Diagnostics -> [Text]
renderDiagnostics input ds =
    [ renderDiagnostic input d
    | d <- sortOn (offsetStart . diagSpan) (diagnosticList ds)
    ]

-- | Offsets at which each line begins; line 1 begins at 0. Lines end at LF,
-- CR or CRLF, which is how the reference counts them.
lineStarts :: Text -> [Int]
lineStarts txt = reverse (go 0 txt [0])
    where
        go off rest acc =
            case T.uncons rest of
                Nothing -> reverse acc
                Just (c, next) ->
                    let len = lineBreakLen c next
                     in if len > 0
                            then go (off + len) (T.drop len next) (off : acc)
                            else go (off + 1) next acc
        lineBreakLen '\r' rest
            | T.isPrefixOf "\n" rest = 2
        lineBreakLen '\n' _ = 1
        lineBreakLen '\r' _ = 1
        lineBreakLen _ _ = 0

toInt :: (Integral a) => a -> Int
toInt = fromIntegral
