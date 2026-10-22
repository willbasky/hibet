-- Ожидания: hewts — AST (REFACTOR-AST шаги 1–3); корпуса — генератор.

module Test.Grammar.Syllable (tests) where

import Convert (SpellItem (..), pSentence)
import Convert.Diagnostic
    ( Diagnostic (..)
    , DiagnosticCode (DuplicateFinal, RepeatedCaret)
    , Diagnostics
    , diagnosticList
    )
import Convert.Grammar.Parser (parseEither, runSpell, takeStacks)
import Convert.Grammar.Structure (pStructure)
import qualified Convert.Grammar.Syllable as Ast
import Convert.Sentence (Syllable (..))
import Convert.Token
    ( ConSpec (..)
    , Consonant (..)
    , FinalMark (..)
    , SubConsonant (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (TsWylie)
    , Vowel (Ve, Vo, Vu)
    , fromSubjoined
    , offsetStart
    , tokenCanonical
    , tokenRaw
    , tokenSource
    , tokenSpan
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List (partition, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar syllable (nested)"
        [ testCase "bkra: prefix stack, then root with a subfix" $ do
            canonicalsW "bkra"
                @?= Right [TcConsonant Cb, TcConsonant Ck, TcConsonant Cr]
            rolesW "bkra" @?= Right [Ast.RolePrefix, Ast.RoleRoot]
        , testCase "rka: the superfix and its root share one stack" $ do
            fmap (map Ast.stRole) (stacksW "rka") @?= Right [Ast.RoleRoot]
            fmap (map rawsOfStack) (stacksW "rka") @?= Right [["r", "k"]]
        , testCase "kab: the suffix opens its own stack" $ do
            fmap (map Ast.stRole) (stacksW "kab")
                @?= Right [Ast.RoleRoot, Ast.RoleSuffix]
            fmap (map rawsOfStack) (stacksW "kab") @?= Right [["k"], ["b"]]
        , testCase "kabs: the postfix opens its own stack" $ do
            fmap (map Ast.stRole) (stacksW "kabs")
                @?= Right [Ast.RoleRoot, Ast.RoleSuffix, Ast.RolePostfix]
            fmap (map rawsOfStack) (stacksW "kabs")
                @?= Right [["k"], ["b"], ["s"]]
        , testCase "བཾྲ: canonical order is root, subfix, final" $ do
            canonicalsU "བཾྲ"
                @?= Right [TcConsonant Cb, TcSubConsonant SCr, TcFinal FMAnusvara]
            fmap (map rawsOfStack) (stacksU "བཾྲ")
                @?= Right [["བ", "ྲ", "ཾ"]]
            fmap
                (map (map tokenRaw . Ast.finalsTokens . Ast.stFinals))
                (stacksU "བཾྲ")
                @?= Right [["ཾ"]]
        , testCase "f: the caret is its own field" $
            canonicalsW "f" @?= Right [TcConsonant Cph, TcFinal FMCaret]
        , testCase "ཧྥ: the subjoined root stays in the base stack" $ do
            fmap (map Ast.stRole) (stacksU "ཧྥ") @?= Right [Ast.RoleRoot]
            fmap (map rawsOfStack) (stacksU "ཧྥ")
                @?= Right [["ཧ", "ྥ"]]
            fmap (map rawsOfStack) (stacksU "ཧྥི")
                @?= Right [["ཧ", "ྥ", "ི"]]
        , testCase "bo+ra: the a beside a written vowel is input syntax" $
            canonicalsW "bo+ra" @?= Right [TcConsonant Cb, TcConsonant Cr, TcVowel Vo]
        , testCase "bru+e: both written vowels stay in one slot" $
            canonicalsW "bru+e"
                @?= Right [TcConsonant Cb, TcConsonant Cr, TcVowel Vu, TcVowel Ve]
        , testCase "g+m+r+a: three subjoined letters stay in the stack" $
            canonicalsW "g+m+r+a"
                @?= Right [TcConsonant Cg, TcConsonant Cm, TcConsonant Cr, TcConsonant Ca]
        , testCase "g.yul / གཡུལ: a dot after the prefix parts the stacks (A2)" $ do
            rolesW "g.yul"
                @?= Right [Ast.RolePrefix, Ast.RoleRoot, Ast.RoleSuffix]
            shapesW "g.yul" @?= shapesU "གཡུལ"
        , testCase "gyi stays one stack, g.yi is a prefix and a root (A2)" $ do
            rolesW "gyi" @?= Right [Ast.RoleRoot]
            rolesW "g.yi" @?= Right [Ast.RolePrefix, Ast.RoleRoot]
            shapesW "gyi" @?= shapesU "གྱི"
            shapesW "g.yi" @?= shapesU "གཡི"
        , testCase "mnga' / མངའ: the bare a-chung in the tail is a suffix (A)" $ do
            rolesW "mnga'" @?= Right [Ast.RolePrefix, Ast.RoleRoot, Ast.RoleSuffix]
            rolesU "མངའ" @?= Right [Ast.RolePrefix, Ast.RoleRoot, Ast.RoleSuffix]
            shapesW "mnga'" @?= shapesU "མངའ"
        , testCase "སྒྲྭ / sgrwa: a superfix over a root with two subfixes (A3)" $ do
            fmap (map Ast.stRole) (stacksU "སྒྲྭ") @?= Right [Ast.RoleRoot]
            fmap (map (fmap tokenRaw . Ast.stSuperfix)) (stacksU "སྒྲྭ")
                @?= Right [Just "ས"]
            fmap (map (fmap tokenRaw . Ast.stRoot)) (stacksU "སྒྲྭ")
                @?= Right [Just "ྒ"]
            fmap (map rawsOfStack) (stacksU "སྒྲྭ")
                @?= Right [["ས", "ྒ", "ྲ", "ྭ"]]
            fmap (map rawsOfStack) (stacksW "sgrwa")
                @?= Right [["s", "g", "r", "w"]]
            shapesW "sgrwa" @?= shapesU "སྒྲྭ"
        , testCase "bkra / བཀྲ and kab / ཀབ: controls keep one AST" $ do
            shapesW "bkra" @?= shapesU "བཀྲ"
            shapesW "kab" @?= shapesU "ཀབ"
        , testCase "the implicit a of a glyph run is a fact of its root stack" $ do
            fmap (map Ast.stVowel) (stacksU "ཀབ")
                @?= Right [Just Ast.VowelImplicit, Nothing]
            fmap (map Ast.stVowel) (stacksW "rk")
                @?= Right [Nothing]
        , testCase "the closing sign of the tail is a suffix (A5)" $ do
            rolesU "པའམ" @?= Right [Ast.RoleRoot, Ast.RoleRoot, Ast.RoleSuffix]
            rolesU "བའམ" @?= Right [Ast.RoleRoot, Ast.RoleRoot, Ast.RoleSuffix]
            rolesU "ཁྱེའུས" @?= Right [Ast.RoleRoot, Ast.RoleRoot, Ast.RoleSuffix]
            rolesU "པའེ" @?= Right [Ast.RoleRoot, Ast.RoleRoot]
            rolesW "pa'am" @?= Right [Ast.RoleRoot, Ast.RoleRoot, Ast.RoleSuffix]
            rolesW "khye'us" @?= Right [Ast.RoleRoot, Ast.RoleRoot, Ast.RoleSuffix]
        , testCase "a prefix its table cannot lead is a root (4.2)" $ do
            rolesW "bdza" @?= Right [Ast.RoleRoot, Ast.RoleRoot]
            rolesW "grla" @?= Right [Ast.RoleRoot, Ast.RoleRoot]
            rolesU "བརྟ" @?= Right [Ast.RolePrefix, Ast.RoleRoot]
        , testCase "corpora: the items cover every run token exactly once" checkCorpora
        , testCase "coverage fails on a duplicate kept token" $
            coverageFails [kTok] [kTok, kTok]
        , testCase "coverage fails on a lost run token" $
            coverageFails [kTok, mTok] [kTok]
        , testCase "coverage fails on a foreign token" $
            coverageFails [kTok] [mTok]
        , testCase "coverage accepts the implicit a of a Wylie run" $
            checkRun [kTok, aTok] [kTok] [] @?= Right ()
        , testCase
            "the structure's returned tokens agree with its stacks"
            checkStructureReturn
        , testCase "Finals: one slot takes one mark" $ do
            let mark = case fst (tokenizeUnicode "ཾ") of
                    [m] -> m
                    _ -> error "expected one anusvara token"
            let once = Ast.snocFinal mark Ast.emptyFinals
            assertBool
                "the first anusvara must fit"
                (case once of Just _ -> True; Nothing -> False)
            assertBool
                "the second anusvara in the same slot must be refused"
                (case once >>= Ast.snocFinal mark of Nothing -> True; Just _ -> False)
        ]

-- | Every run of both spellings, one item slice at a time: the run ends where
-- the next item begins, so the signs a claimed run swallowed into its tail
-- stay with that run. 'checkRun' proves the slice is covered exactly.
checkCorpora :: Assertion
checkCorpora = do
    wylie <- readInputs tokenizeWylie wylieColumn wylieFiles
    unicodeVectors <- readInputs tokenizeUnicode unicodeColumn wylieFiles
    kangyur <- readInputs tokenizeUnicode id ["test/vectors/kang.txt"]
    checkSide "Wylie" wylie
    checkSide "Unicode" (kangyur <> unicodeVectors)
    where
        checkSide side inputs =
            let bad =
                    [ (input, err)
                    | (input, toks) <- inputs
                    , let items = either (error . T.unpack) id (parseEither pSentence toks)
                    , Left err <- [checkItems toks items]
                    ]
             in assertBool
                    (side <> ": run tokens uncovered:\n" <> unlines (map show bad))
                    (null bad)

-- | The run of an item is the slice from the previous item's end to the next
-- item's first token: a run that is not a syllable item holds its whole slice,
-- while a claimed run may leave signs on the allowlist behind.
checkItems :: [Token] -> [SpellItem] -> Either T.Text ()
checkItems input = go input
    where
        go rest [] =
            if null rest
                then Right ()
                else Left ("tokens after the last item: " <> raws rest)
        go rest (item : more) =
            let (slice, after) = case more of
                    [] -> (rest, [])
                    (next : _) -> break (== firstToken next) rest
             in checkRun slice (itemTokens item) (itemDiags item) *> go after more

        firstToken item = case itemTokens item of
            (t : _) -> t
            [] -> error "an item without tokens"

-- | Every token of a run is either held by an item or on the exact allowlist:
-- a stack separator (input syntax, §8 B2), the Wylie letter @a@ that spells
-- the implicit vowel (a sign that never prints), or a final mark the run's own
-- findings name - a duplicate final or a second caret. Nothing else may
-- vanish, and an item token the run does not hold fails as well.
checkRun :: [Token] -> [Token] -> [Diagnostic] -> Either T.Text ()
checkRun input kept diags = do
    -- Stack order is canonical, run order is by span: the coverage rule is
    -- about which tokens are held, so the kept list is put back in run order.
    dropped <- coverRun input (sortOn (offsetStart . tokenSpan) kept)
    let (_, noSeps) = partition isSeparator dropped
        (_, noImplicit) = partition isImplicitA noSeps
        (finals, other) = partition isFinal noImplicit
    case other of
        [] -> checkFinals finals
        _ -> Left ("signs that no item holds: " <> raws other)
    where
        checkFinals finals
            | length carets /= repeatedCarets =
                Left ("dropped carets without a finding: " <> raws carets)
            | sortOn id droppedMarks /= sortOn id blamedMarks =
                Left
                    ( "dropped finals and their findings differ: dropped "
                        <> T.intercalate ", " droppedMarks
                        <> ", blamed "
                        <> T.intercalate ", " blamedMarks
                    )
            | otherwise = Right ()
            where
                carets = filter isCaret finals
                repeatedCarets =
                    length [d | d <- diags, diagCode d == RepeatedCaret]
                droppedMarks = map tokenRaw (filter (not . isCaret) finals)
                blamedMarks =
                    [ quoted (diagMessage d)
                    | d <- diags
                    , diagCode d == DuplicateFinal
                    ]

        -- The duplicate-final wording quotes the mark it dropped.
        quoted msg = case T.splitOn "\"" msg of
            (_ : mark : _) -> mark
            _ -> ""

-- | Match the tokens an item holds against its run, in source order. A kept
-- token that is nowhere left in the run is a foreign token or a duplicate; the
-- run tokens left unmatched are the signs the grammar dropped.
coverRun :: [Token] -> [Token] -> Either T.Text [Token]
coverRun input kept = go [] input kept
    where
        go dropped [] remaining
            | null remaining = Right (reverse dropped)
            | otherwise =
                Left
                    ( "item tokens the run does not hold: "
                        <> raws remaining
                    )
        go dropped (t : ts) remaining = case remaining of
            (k : ks) | k == t -> go dropped ts ks
            _ -> go (t : dropped) ts remaining

coverageFails :: [Token] -> [Token] -> Assertion
coverageFails input kept = case checkRun input kept [] of
    Left _ -> pure ()
    Right () ->
        assertFailure
            ("coverage accepted a broken run: kept " <> T.unpack (raws kept))

-- | The structure hands back the signs it read as a flat 'Seq Token' (4e-C),
-- and the sentence does not use that list today. The stacks must tell the same
-- story: every stacked sign is in the returned stream, and the only extras are
-- the implicit @a@ signs of Wylie (input syntax, no slot). Both the returned
-- stream and the stacks are put back in run order by span, because a stack
-- writes its signs in canonical order and the return in construction order.
checkStructureReturn :: Assertion
checkStructureReturn = do
    wylie <- readInputs tokenizeWylie wylieColumn wylieFiles
    unicodeVectors <- readInputs tokenizeUnicode unicodeColumn wylieFiles
    kangyur <- readInputs tokenizeUnicode id ["test/vectors/kang.txt"]
    let inputs = wylie <> unicodeVectors <> kangyur
        bad =
            [ (input, err)
            | (input, toks) <- inputs
            , Left err <- [checkReturned toks]
            ]
    assertBool
        ("the structure's return and its stacks disagree:\n" <> unlines (map show bad))
        (null bad)
    where
        checkReturned toks =
            case parseEither (runSpell ((,) <$> pStructure <*> takeStacks)) toks of
                Left _ -> Right () -- no structure claims this run: an invalid item
                Right (returned, stacks) -> do
                    let sortedReturned =
                            sortOn (offsetStart . tokenSpan) (toList returned)
                        flatSorted =
                            sortOn
                                (offsetStart . tokenSpan)
                                (toList (Ast.flatten stacks))
                    -- Read order, put back by span: a long vowel's two halves
                    -- share a zero-width span, and the stable sort keeps their
                    -- order. A kept token with no match is foreign or repeated.
                    _ <- coverRun toks sortedReturned
                    extras <- coverRun sortedReturned flatSorted
                    case filter (not . isImplicitA) extras of
                        [] -> Right ()
                        bad ->
                            Left
                                ( "returned signs that are neither stacked nor the implicit a: "
                                    <> raws bad
                                )

isSeparator :: Token -> Bool
isSeparator tok = case tokenCanonical tok of
    TcConSpec CSDot -> True
    TcConSpec CSPlus -> True
    _ -> False

-- | The bare Wylie @a@ that spells the implicit vowel. A Tibetan a-chen is a
-- real letter with a slot of its own, so it is not on this list.
isImplicitA :: Token -> Bool
isImplicitA tok = case tokenCanonical tok of
    TcConsonant Ca -> tokenSource tok == TsWylie
    _ -> False

isFinal :: Token -> Bool
isFinal tok = case tokenCanonical tok of
    TcFinal _ -> True
    _ -> False

isCaret :: Token -> Bool
isCaret tok = tokenCanonical tok == TcFinal FMCaret

raws :: [Token] -> T.Text
raws = T.intercalate ", " . map tokenRaw

-- | The signs the three negative coverage cases are built from.
probeTokens :: [Token]
probeTokens = fst (tokenizeWylie "kaM")

kTok, aTok, mTok :: Token
kTok = probeTokens !! 0
aTok = probeTokens !! 1
mTok = probeTokens !! 2

-- | Every token an item holds, in the order the parser read them.
itemTokens :: SpellItem -> [Token]
itemTokens (SyllableItem s) = toList (Ast.flatten (sylStacks s))
itemTokens (InvalidSyllableItem ts _) = ts
itemTokens (Number ts) = ts
itemTokens (Punct ts) = ts
itemTokens (Other ts) = ts

-- | The warnings that came with an item; only a syllable carries findings
-- today (the marks of its own run).
itemDiags :: SpellItem -> [Diagnostic]
itemDiags (SyllableItem s) = diagnosticList (sylDiags s)
itemDiags (InvalidSyllableItem _ ds) = diagnosticList ds
itemDiags _ = []

wylieFiles :: [FilePath]
wylieFiles =
    [ "test/vectors/jsewts_parity.tsv"
    , "test/vectors/lingua_parity.tsv"
    , "test/vectors/ewts_js_parity.tsv"
    , "test/vectors/ewts_rs_rules.tsv"
    , "test/vectors/java_ewts.tsv"
    ]

readInputs ::
    (T.Text -> ([Token], Diagnostics))
    -> (T.Text -> T.Text)
    -> [FilePath]
    -> IO [(T.Text, [Token])]
readInputs tokenize column paths = fmap concat (mapM one paths)
    where
        one path = do
            text <- readUtf8File path
            pure
                [ (input, fst (tokenize input))
                | line <- T.lines text
                , let clean = T.dropWhile (== '\xfeff') line
                , not (T.null clean)
                , not ("#" `T.isPrefixOf` clean)
                , let input = column clean
                , not (T.null input)
                ]

wylieColumn :: T.Text -> T.Text
wylieColumn = T.takeWhile (/= '\t')

-- | The Unicode column of a parity row.
unicodeColumn :: T.Text -> T.Text
unicodeColumn line = case T.splitOn "\t" line of
    (_ : u : _) -> u
    _ -> line

canonicalsW :: T.Text -> Either T.Text [TokenCanonical]
canonicalsW = fmap (concatMap (map tokenCanonical . tokensOf)) . parseRuns tokenizeWylie

canonicalsU :: T.Text -> Either T.Text [TokenCanonical]
canonicalsU = fmap (concatMap (map tokenCanonical . tokensOf)) . parseRuns tokenizeUnicode

rolesW :: T.Text -> Either T.Text [Ast.Role]
rolesW = fmap (map Ast.stRole . concatMap stacksOf) . parseRuns tokenizeWylie

rolesU :: T.Text -> Either T.Text [Ast.Role]
rolesU = fmap (map Ast.stRole . concatMap stacksOf) . parseRuns tokenizeUnicode

-- | The form of a syllable, compared across the two spellings: the role of
-- every stack and the canonical signs it holds. The implicit @a@ is input
-- syntax (Wylie spells it with a letter, the glyphs write nothing) and has no
-- token, so 'Ast.stackTokens' already leaves it out of the comparison.
shapesW :: T.Text -> Either T.Text [(Ast.Role, [TokenCanonical])]
shapesW = fmap (map shape . concatMap stacksOf) . parseRuns tokenizeWylie

shapesU :: T.Text -> Either T.Text [(Ast.Role, [TokenCanonical])]
shapesU = fmap (map shape . concatMap stacksOf) . parseRuns tokenizeUnicode

shape :: Ast.Stack -> (Ast.Role, [TokenCanonical])
shape stack =
    (Ast.stRole stack, map letter (toList (Ast.stackTokens stack)))
    where
        -- A subjoined glyph and the bare Wylie letter of the same stack carry
        -- different canonical classes; the form compares the letters themselves.
        letter tok = case tokenCanonical tok of
            TcSubConsonant sc -> TcConsonant (fromSubjoined sc)
            canonical -> canonical

stacksW :: T.Text -> Either T.Text [Ast.Stack]
stacksW = fmap (concatMap stacksOf) . parseRuns tokenizeWylie

stacksU :: T.Text -> Either T.Text [Ast.Stack]
stacksU = fmap (concatMap stacksOf) . parseRuns tokenizeUnicode

rawsOfStack :: Ast.Stack -> [T.Text]
rawsOfStack = map tokenRaw . toList . Ast.stackTokens

tokensOf :: Syllable -> [Token]
tokensOf = toList . Ast.flatten . sylStacks

stacksOf :: Syllable -> [Ast.Stack]
stacksOf = toList . sylStacks

parseRuns ::
    (T.Text -> ([Token], Diagnostics)) -> T.Text -> Either T.Text [Syllable]
parseRuns tokenize input =
    fmap
        ( \items ->
            [s | SyllableItem s <- items]
        )
        (parseEither pSentence (fst (tokenize input)))

-- | Read a vector file as UTF-8 regardless of the locale, so the corpora decode
-- identically everywhere. Same helper as in Test.Parity, kept local.
readUtf8File :: FilePath -> IO T.Text
readUtf8File path = do
    bytes <- BS.readFile path
    case TE.decodeUtf8' bytes of
        Left err -> error (path ++ ": not valid UTF-8: " ++ show err)
        Right text -> pure text
