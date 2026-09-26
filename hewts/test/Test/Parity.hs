module Test.Parity (tests) where

-- \| Parity against the reference corpora (see @test/vectors/README.md@).
--
-- Every corpus file uses the reference's own six-column layout:
--
-- > wylie <TAB> unicode <TAB> warns_w2u <TAB> wylie_back <TAB> warns_u2w <TAB> roundtrip_differs
--
-- An expectation the reference does not state is either missing or written as
-- @?@; such fields are not checked. Absence never means "zero".
--
-- The suite is a ratchet: @parity_baseline.tsv@ records how many checks of
-- each kind passed when it was written, and the tests fail when the current
-- numbers fall below it. Progress in the later waves means lowering the
-- baseline, not deleting the corpora. @test/vectors/parity_report.txt@ always
-- holds the current full list of differences.

import Convert (OutputFormat (..), SpellItem (..), pSentence, renderItems)
import Convert.Diagnostic (Diagnostics, renderDiagnostics)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Token (Token)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "parity"
        [ testCase "corpus sizes" checkSizes
        , testCase "jsewts 210" (checkCorpus CJsewts)
        , testCase "lingua 218" (checkCorpus CLingua)
        , testCase "ewts-rs rules 16" (checkCorpus CRules)
        , testCase "java EWTS 6" (checkCorpus CJava)
        , testCase "kangyur round-trip 254" (checkCorpus CKang)
        , testCase "report file" writeReport
        ]

data Corpus
    = CJsewts
    | CLingua
    | CRules
    | CJava
    | CKang
    deriving (Eq, Ord, Show, Enum, Bounded)

data Kind
    = KW2U
    | KU2W
    | KRoundtrip
    | KW2UWarns
    | KU2WWarns
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | What a reference states about one corpus.
data Case = Case
    { caseLine :: !Int
    , caseNote :: !(Maybe Text)
    , caseWylie :: !Text
    , caseUnicode :: !Text
    , caseWylieBack :: !(Maybe Text)
    , caseWarnsU2W :: !(Maybe Int)
    , caseRoundtripDiffers :: !(Maybe Bool)
    }

data Outcome
    = Pass
    | Fail !Text
    deriving (Eq, Show)

data Item = Item
    { itemLabel :: !Text
    , itemKind :: !Kind
    , itemOutcome :: !Outcome
    }

-- | The whole corpus as check items; only the checks the reference actually
-- specifies are produced.
itemsFor :: Corpus -> IO [Item]
itemsFor CKang = kangItems
itemsFor corpus = do
    text <- readUtf8File (corpusFile corpus)
    warns <- readWarns (warnsFile corpus)
    pure (concatMap (caseItems warns) (readCorpus text))
    where
        caseItems warns c =
            concat
                [ [Item (label c) KW2U (checkW2U c)]
                , [Item (label c) KU2W (checkU2W c) | caseWylieBack c /= Nothing]
                , [ Item (label c) KRoundtrip (checkRoundtrip c) | caseRoundtripDiffers c /= Nothing
                  ]
                , [Item (label c) KW2UWarns (checkW2UWarns warns c) | hasWarnTexts corpus]
                , [Item (label c) KU2WWarns (checkU2WWarns c) | caseWarnsU2W c /= Nothing]
                ]

-- | The Kangyur corpus states no expected output: the reference checks itself
-- (U->W->U), so we check our own round-trip on real text.
kangItems :: IO [Item]
kangItems = do
    text <- readUtf8File (corpusFile CKang)
    pure
        [ Item
            ("line " <> T.pack (show n) <> " " <> oneLine (T.take 20 raw))
            KRoundtrip
            (checkKangLine raw)
        | (n, raw) <- zip [1 :: Int ..] (T.lines (dropBom text))
        , not (T.null raw)
        ]

corpusFile :: Corpus -> FilePath
corpusFile corpus =
    case corpus of
        CJsewts -> "test/vectors/jsewts_parity.tsv"
        CLingua -> "test/vectors/lingua_parity.tsv"
        CRules -> "test/vectors/ewts_rs_rules.tsv"
        CJava -> "test/vectors/java_ewts.tsv"
        CKang -> "test/vectors/kang.txt"

-- | The warning TEXTS the reference produced for the same rows, exported once
-- from the reference itself (the corpus itself only stores counts). The
-- non-strict mode is the one we compare against: the strict-only warnings
-- belong to the modes, which arrive with the later waves.
warnsFile :: Corpus -> Maybe FilePath
warnsFile corpus =
    case corpus of
        CJsewts -> Just "test/vectors/jsewts_warns.tsv"
        CLingua -> Just "test/vectors/lingua_warns.tsv"
        _ -> Nothing

hasWarnTexts :: Corpus -> Bool
hasWarnTexts corpus = warnsFile corpus /= Nothing

-- | Exported warning texts by corpus row; a row with only a number has none.
readWarns :: Maybe FilePath -> IO (Map Int [Text])
readWarns Nothing = pure M.empty
readWarns (Just path) = do
    text <- readUtf8File path
    pure (M.fromList (mapMaybe row (T.lines text)))
    where
        row line
            | T.null line || "#" `T.isPrefixOf` line = Nothing
            | otherwise =
                case T.splitOn "\t" line of
                    (n : rest) | Just i <- readNumber n -> Just (i, rest)
                    _ -> Nothing
        readNumber t
            | not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t =
                Just (read (T.unpack t))
            | otherwise = Nothing

baselineFile :: FilePath
baselineFile = "test/vectors/parity_baseline.tsv"

reportFile :: FilePath
reportFile = "test/vectors/parity_report.txt"

-- | A truncated or edited copy of a corpus must not silently shrink the
-- sample, so the sizes are pinned.
expectedSize :: Corpus -> Int
expectedSize corpus =
    case corpus of
        CJsewts -> 210
        CLingua -> 218
        CRules -> 16
        CJava -> 6
        -- \| @kang.txt@ starts with a UTF-8 BOM that sits on a line of its own,
        -- so the file holds 254 real lines (255 newlines, first line = BOM).
        CKang -> 254

-- | @kang.txt@ starts with a UTF-8 BOM; strip it before the first line.
dropBom :: Text -> Text
dropBom = T.dropWhile (== '\xfeff')

-- | Read a vector file as UTF-8 regardless of the locale, so the corpora
-- decode identically everywhere.
readUtf8File :: FilePath -> IO Text
readUtf8File path = do
    bytes <- BS.readFile path
    case TE.decodeUtf8' bytes of
        Left err -> error (path ++ ": not valid UTF-8: " <> show err)
        Right text -> pure text

-- | Parse one corpus file. Row and column order is the reference's. A comment
-- line above a row names it where there is one - the ewts-rs rules corpus
-- labels its rows "@Rule N@", and that reads better in the report than the
-- line number of a file we generated ourselves.
readCorpus :: Text -> [Case]
readCorpus txt = zipWith applyNote (rowNotes txt) (mapMaybe row (zip [1 ..] (T.lines txt)))
    where
        -- one note per data row: a comment line sets the note, a data row takes it
        rowNotes t = reverse (snd (foldl' step (Nothing, []) (T.lines t)))
        step (note, acc) line
            | T.null line = (note, acc)
            | "#" `T.isPrefixOf` line = (T.stripPrefix "# Rule " (T.strip line), acc)
            | otherwise = (note, note : acc)
        applyNote note c = c{caseNote = note}
        row (n, line)
            | T.null line = Nothing
            | "#" `T.isPrefixOf` line = Nothing
            | otherwise =
                case T.splitOn "\t" line of
                    [w, u] -> Just (Case n Nothing w u Nothing Nothing Nothing)
                    [w, u, _ws] -> Just (Case n Nothing w u Nothing Nothing Nothing)
                    [w, u, _ws, b] -> Just (Case n Nothing w u (Just b) Nothing Nothing)
                    [w, u, _ws, b, wsb] -> Just (Case n Nothing w u (Just b) (count wsb) Nothing)
                    [w, u, _ws, b, wsb, rt] -> Just (Case n Nothing w u (Just b) (count wsb) (differs rt))
                    fields ->
                        error
                            ( "parity corpus: row "
                                <> show n
                                <> " has "
                                <> show (length fields)
                                <> " fields, expected 2..6: "
                                <> show line
                            )

-- | A numeric expectation; anything else (@?@) means "not specified".
count :: Text -> Maybe Int
count t
    | not (T.null t) && T.all isDigit t = Just (read (T.unpack t))
    | otherwise = Nothing
    where
        isDigit c = c >= '0' && c <= '9'

-- | The round-trip flag: @0@ means the reference round-trips back to the same
-- Unicode, anything else numeric means it differs.
differs :: Text -> Maybe Bool
differs "0" = Just False
differs t
    | not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t = Just True
    | otherwise = Nothing

-- | Wylie input parses under the Wylie arms of the grammar (the @a@ is written
-- and consumed as the implicit vowel), Unicode input under the Tibetan arms.
convertW2U :: Text -> (Either Text Text, Diagnostics)
convertW2U input = (fmap (renderItems OutUnicode) (parseItems Wylie tokens), diags)
    where
        (tokens, diags) = tokenizeWylie input

convertU2W :: Text -> (Either Text Text, Diagnostics)
convertU2W input = (fmap (renderItems OutWylie) (parseItems Tibetan tokens), diags)
    where
        (tokens, diags) = tokenizeUnicode input

parseItems :: Spelling -> [Token] -> Either Text [SpellItem]
parseItems spelling = parseEither (pSentence spelling)

-- | Wylie -> Unicode -> Wylie -> Unicode, all with our own converter.
roundTripW2U :: Text -> Either Text Text
roundTripW2U w = do
    unicode <- fst (convertW2U w)
    wylie <- fst (convertU2W unicode)
    fst (convertW2U wylie)

roundTripU2W :: Text -> Either Text Text
roundTripU2W u = do
    wylie <- fst (convertU2W u)
    fst (convertW2U wylie)

checkW2U :: Case -> Outcome
checkW2U c =
    case fst (convertW2U (caseWylie c)) of
        Left err -> Fail ("parse error: " <> err)
        Right out
            | out == caseUnicode c -> Pass
            | otherwise -> Fail ("got " <> quoted out <> " want " <> quoted (caseUnicode c))

checkU2W :: Case -> Outcome
checkU2W c =
    case fst (convertU2W (caseUnicode c)) of
        Left err -> Fail ("parse error: " <> err)
        Right out
            | out == want -> Pass
            | otherwise -> Fail ("got " <> quoted out <> " want " <> quoted want)
    where
        want = fromMaybe "" (caseWylieBack c)

-- | The warning messages we produce must match the reference's, text and all.
checkW2UWarns :: Map Int [Text] -> Case -> Outcome
checkW2UWarns warns c =
    case M.lookup (caseLine c) warns of
        Nothing -> Fail "no exported warning texts for this row"
        Just expected
            | actual == expected -> Pass
            | otherwise -> Fail (listDiff actual expected)
    where
        actual = renderDiagnostics (caseWylie c) (snd (convertW2U (caseWylie c)))

-- | For the back conversion the corpus records only whether the reference
-- warned at all, and that is all we can check.
checkU2WWarns :: Case -> Outcome
checkU2WWarns c =
    case (caseWarnsU2W c, length actual) of
        (Just 0, 0) -> Pass
        (Just n, got)
            | n > 0 && got > 0 -> Pass
        _ ->
            Fail
                ( T.pack
                    ( "want "
                        <> show (caseWarnsU2W c)
                        <> " warning(s), got "
                        <> show (length actual)
                    )
                )
    where
        actual = renderDiagnostics (caseUnicode c) (snd (convertU2W (caseUnicode c)))

listDiff :: [Text] -> [Text] -> Text
listDiff actual expected =
    "got "
        <> T.unwords (map quoted actual)
        <> " want "
        <> T.unwords (map quoted expected)

checkRoundtrip :: Case -> Outcome
checkRoundtrip c =
    case roundTripW2U (caseWylie c) of
        Left err -> Fail ("parse error: " <> err)
        Right out -> case caseRoundtripDiffers c of
            Just False
                | out == caseUnicode c -> Pass
                | otherwise ->
                    Fail
                        ("should round-trip, got " <> quoted out <> " want " <> quoted (caseUnicode c))
            Just True
                | out /= caseUnicode c -> Pass
                | otherwise -> Fail ("should not round-trip, but got " <> quoted out)
            Nothing -> Fail "corpus row has no round-trip flag"

-- | Mirror the reference's own normalising before comparing Kangyur lines:
-- drop ASCII spaces and expand the double shad. jsewts normalises both sides
-- of the comparison, so we must too — otherwise every ༎ reads as a
-- regression.
checkKangLine :: Text -> Outcome
checkKangLine raw =
    case roundTripU2W (normalise raw) of
        Left err -> Fail ("parse error: " <> err)
        Right out
            | normalise out == want -> Pass
            | otherwise ->
                Fail ("got " <> quoted (normalise out) <> " want " <> quoted want)
    where
        want = normalise raw
        normalise = T.replace "\x0f0e" "\x0f0d\x0f0d" . T.filter (/= ' ')

-- | One line of the reference's own corpus, named by its rule where the corpus
-- names it, and by its line otherwise.
label :: Case -> Text
label c =
    maybe byLine (\n -> "rule " <> n) (caseNote c) <> " " <> oneLine (caseWylie c)
    where
        byLine = "line " <> T.pack (show (caseLine c))

oneLine :: Text -> Text
oneLine t = T.unwords (T.words t)

quoted :: Text -> Text
quoted t
    | T.null t = "<empty>"
    | T.any (== '\t') t = "<" <> T.unwords (T.words t) <> ">"
    | otherwise = t

kindName :: Kind -> String
kindName kind =
    case kind of
        KW2U -> "w2u"
        KU2W -> "u2w"
        KRoundtrip -> "roundtrip"
        KW2UWarns -> "w2u-warnings"
        KU2WWarns -> "u2w-warnings"

corpusName :: Corpus -> String
corpusName corpus =
    case corpus of
        CJsewts -> "jsewts"
        CLingua -> "lingua"
        CRules -> "ewts-rs"
        CJava -> "java"
        CKang -> "kangyur"

-- | The recorded floor: one row per corpus and check kind.
data Baseline = Baseline
    { baseCorpus :: !Corpus
    , baseKind :: !Kind
    , baseTotal :: !Int
    , basePass :: !Int
    }

readBaseline :: IO [Baseline]
readBaseline = do
    text <- readUtf8File baselineFile
    pure
        [ Baseline c k total passed
        | line <- T.lines text
        , not ("#" `T.isPrefixOf` line)
        , not (T.null line)
        , let fs = T.splitOn "\t" line
        , Just (c, k, total, passed) <- [row fs]
        ]
    where
        row :: [Text] -> Maybe (Corpus, Kind, Int, Int)
        row fs = do
            c <- lookupCorpus (fs !! 0)
            k <- lookupKind (fs !! 1)
            pure (c, k, readInt (fs !! 2), readInt (fs !! 3))
        readInt :: Text -> Int
        readInt t =
            case [n | (n, rest) <- reads (T.unpack t), all (== ' ') rest] of
                (n : _) -> n
                [] -> error ("parity baseline: expected a number, got " <> show t)

lookupCorpus :: Text -> Maybe Corpus
lookupCorpus t = lookup (T.unpack t) [(corpusName c, c) | c <- [minBound .. maxBound]]

lookupKind :: Text -> Maybe Kind
lookupKind t = lookup (T.unpack t) [(kindName k, k) | k <- [minBound .. maxBound]]

-- | A corpus plus its checks, one group per check kind that actually applies
-- (a kind the corpus says nothing about is not a group, and needs no baseline
-- row).
grouped :: [Item] -> [(Kind, [Item])]
grouped items =
    [ (kind, [item | item <- items, itemKind item == kind])
    | kind <- [minBound .. maxBound]
    , any ((== kind) . itemKind) items
    ]

counts :: [Item] -> (Int, Int)
counts items = (length items, length [() | item <- items, itemOutcome item == Pass])

checkSizes :: Assertion
checkSizes = mapM_ one [minBound .. maxBound]
    where
        one corpus = do
            items <- itemsFor corpus
            let seen = length (dedup [itemLabel item | item <- items])
            assertBool
                ( "corpus size changed for "
                    <> corpusName corpus
                    <> ": found "
                    <> show seen
                    <> " cases, expected "
                    <> show (expectedSize corpus)
                )
                (seen == expectedSize corpus)
        dedup = foldl' (\acc x -> if x `elem` acc then acc else acc <> [x]) []

-- | Ratchet: never fewer passing checks than recorded, and the totals must
-- still match the corpus.
checkCorpus :: Corpus -> Assertion
checkCorpus corpus = do
    items <- itemsFor corpus
    baseline <- readBaseline
    mapM_ (ratchet corpus baseline) (grouped items)
    where
        ratchet c baselines (kind, group) = do
            let (total, passed) = counts group
                recorded = [b | b <- baselines, baseCorpus b == c, baseKind b == kind]
            case recorded of
                [] ->
                    assertBool
                        ( "no baseline row for "
                            <> corpusName c
                            <> " / "
                            <> kindName kind
                            <> " ("
                            <> show total
                            <> " checks) — add it to "
                            <> baselineFile
                        )
                        False
                [b] -> do
                    assertBool
                        ( "check count changed for "
                            <> corpusName c
                            <> " / "
                            <> kindName kind
                            <> ": "
                            <> show total
                            <> " now, "
                            <> show (baseTotal b)
                            <> " in the baseline"
                        )
                        (total == baseTotal b)
                    let regressions = [item | item <- group, itemOutcome item /= Pass]
                    assertBool
                        ( unlines
                            [ "parity regression in "
                                <> corpusName c
                                <> " / "
                                <> kindName kind
                                <> ": "
                                <> show passed
                                <> "/"
                                <> show total
                                <> " pass, baseline is "
                                <> show (basePass b)
                            , unlines (map (unwords . describe) (take 20 regressions))
                            , "full list: " <> reportFile
                            ]
                        )
                        (passed >= basePass b)
                _ -> assertBool "duplicate baseline rows" False
        describe item =
            [ T.unpack (itemLabel item)
            , case itemOutcome item of
                Pass -> "unexpectedly PASS"
                Fail reason -> T.unpack reason
            ]

-- | Regenerate the human-readable report; this is the "scales" output.
writeReport :: Assertion
writeReport = do
    corpusItems <- mapM (\c -> (c,) <$> itemsFor c) [minBound .. maxBound]
    baseline <- readBaseline
    TIO.writeFile reportFile (T.unlines (render corpusItems baseline))
    written <- TIO.readFile reportFile
    assertBool ("report file " <> reportFile <> " is empty") (not (T.null written))
    where
        render corpusItems baselines = concatMap (one baselines) corpusItems
            where
                one bs (c, items) =
                    [ T.pack (corpusName c)
                        <> " ("
                        <> T.pack (show (length (dedup [itemLabel item | item <- items])))
                        <> " cases)"
                    ]
                        <> concatMap (row bs c) (grouped items)
                        <> [""]
                row bs c (kind, group) =
                    let (total, passed) = counts group
                        floor' = fromMaybe "-" (T.pack . show . basePass <$> lookupBase bs c kind)
                     in [ "  "
                            <> T.pack (kindName kind)
                            <> "  "
                            <> T.pack (show passed)
                            <> "/"
                            <> T.pack (show total)
                            <> "  (baseline "
                            <> floor'
                            <> ")"
                        , T.unlines
                            [ "    " <> itemLabel item <> "  " <> reason item
                            | item <- group
                            , itemOutcome item /= Pass
                            ]
                        ]
                reason item =
                    case itemOutcome item of
                        Pass -> "unexpectedly PASS"
                        Fail r -> r
                dedup = foldl' (\acc x -> if x `elem` acc then acc else acc <> [x]) []
        lookupBase baselines c k =
            case [b | b <- baselines, baseCorpus b == c, baseKind b == k] of
                [b] -> Just b
                _ -> Nothing
