-- | Non-fatal diagnostics: what the converter noticed while reading input,
-- following the reference's own message formats.
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

-- | @Unexpected character "x".@ - the reference's wording for a letter or
-- special marker that occurs where nothing expects it.
unexpectedCharacter :: Span -> Char -> Diagnostic
unexpectedCharacter sp c =
    Diagnostic
        UnexpectedCharacter
        SevWarning
        sp
        Nothing
        ("Unexpected character \"" <> T.singleton c <> "\".")

-- | @Unfinished [non-Wylie stuff].@ - a bracketed foreign-text block that is
-- never closed; the reference reports it and stops reading.
unfinishedComment :: Span -> Diagnostic
unfinishedComment sp =
    Diagnostic
        UnfinishedComment
        SevWarning
        sp
        Nothing
        "Unfinished [non-Wylie stuff]."

-- | @"\u01x3": invalid hex code.@ - a \\uXXXX escape whose code is not a valid
-- hexadecimal number. The reference drops such an escape entirely, and so do
-- we; the message quotes the escape exactly as it was written.
invalidHexCode :: Span -> Text -> Diagnostic
invalidHexCode sp raw =
    Diagnostic
        InvalidHexCode
        SevWarning
        sp
        Nothing
        ("\"" <> raw <> "\": invalid hex code.")

-- | One message in the reference's format: @line N: "word": message@, where
-- the word is quoted only when the diagnostic blames a specific word.
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
