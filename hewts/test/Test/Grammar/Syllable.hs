-- Ожидания: hewts — AST (REFACTOR-AST шаги 1–3); корпуса — генератор.

module Test.Grammar.Syllable (tests) where

import Convert (SpellItem (..), pSentence)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import qualified Convert.Grammar.Syllable as Ast
import Convert.Sentence (Syllable (..))
import Convert.Token
    ( Consonant (..)
    , FinalMark (..)
    , SubConsonant (..)
    , Token
    , TokenCanonical (..)
    , Vowel (Ve, Vo, Vu)
    , fromSubjoined
    , tokenCanonical
    , tokenRaw
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

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
        , testCase "corpora: no run token is lost to the stacks" checkCorpora
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

-- | Every run of both spellings: the tokens the parser kept must appear in
-- the items' stacks in source order, and every token that did not reach a
-- stack must be one the parse is allowed to leave out - a stack separator
-- (@.@, @+@, input syntax) or a final mark the grammar took as a duplicate.
-- Boundaries are 'Punct' items of their own now, so they are in the stream.
checkCorpora :: Assertion
checkCorpora = do
    wylie <- readInputs Wylie wylieColumn wylieFiles
    unicodeVectors <- readInputs Tibetan unicodeColumn wylieFiles
    kangyur <- readInputs Tibetan id ["test/vectors/kang.txt"]
    checkSide "Wylie" wylie
    checkSide "Unicode" (kangyur <> unicodeVectors)
    where
        checkSide side inputs = do
            let lost =
                    [ (input, missing)
                    | (input, toks) <- inputs
                    , let Right items = parseEither (pSentence (spellingOf side)) toks
                          kept = toListOfItems items
                    , let missing = dropped toks kept
                    , not (all allowed missing)
                    ]
            assertBool
                (side <> ": tokens lost to the stacks:\n" <> unlines (map show lost))
                (null lost)

        spellingOf "Wylie" = Wylie
        spellingOf _ = Tibetan

        -- A separator is input syntax (§8 B2), a final mark may be the one
        -- the chain refused (Q6), and the implicit @a@ of a Wylie run is a
        -- fact of the stack, not a token (§4, invariant 5).
        allowed tok = case tokenCanonical tok of
            TcConSpec _ -> True
            TcFinal _ -> True
            TcConsonant Ca -> True
            _ -> False

        -- The tokens the item stream no longer holds, in source order.
        dropped :: [Token] -> [Token] -> [Token]
        dropped input kept = foldl takeOut input kept
        takeOut [] _ = []
        takeOut (t : ts) k
            | t == k = ts
            | otherwise = t : takeOut ts k
        toListOfItems = concatMap itemTokens

-- | Every token an item holds, in the order the parser read them.
itemTokens :: SpellItem -> [Token]
itemTokens (SyllableItem s) = toList (Ast.flatten (sylStacks s))
itemTokens (InvalidSyllableItem ts _) = ts
itemTokens (Number ts) = ts
itemTokens (Punct ts) = ts
itemTokens (Other ts) = ts

wylieFiles :: [FilePath]
wylieFiles =
    [ "test/vectors/jsewts_parity.tsv"
    , "test/vectors/lingua_parity.tsv"
    , "test/vectors/ewts_js_parity.tsv"
    , "test/vectors/ewts_rs_rules.tsv"
    , "test/vectors/java_ewts.tsv"
    ]

readInputs ::
    Spelling
    -> (T.Text -> T.Text)
    -> [FilePath]
    -> IO [(T.Text, [Token])]
readInputs spelling column paths = fmap concat (mapM one paths)
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
        tokenize = case spelling of
            Wylie -> tokenizeWylie
            Tibetan -> tokenizeUnicode

wylieColumn :: T.Text -> T.Text
wylieColumn = T.takeWhile (/= '\t')

-- | The Unicode column of a parity row.
unicodeColumn :: T.Text -> T.Text
unicodeColumn line = case T.splitOn "\t" line of
    (_ : u : _) -> u
    _ -> line

canonicalsW :: T.Text -> Either T.Text [TokenCanonical]
canonicalsW = fmap (concatMap (map tokenCanonical . tokensOf)) . parseRuns Wylie

canonicalsU :: T.Text -> Either T.Text [TokenCanonical]
canonicalsU = fmap (concatMap (map tokenCanonical . tokensOf)) . parseRuns Tibetan

rolesW :: T.Text -> Either T.Text [Ast.Role]
rolesW = fmap (map Ast.stRole . concatMap stacksOf) . parseRuns Wylie

rolesU :: T.Text -> Either T.Text [Ast.Role]
rolesU = fmap (map Ast.stRole . concatMap stacksOf) . parseRuns Tibetan

-- | The form of a syllable, compared across the two spellings: the role of
-- every stack and the canonical signs it holds. The implicit @a@ is input
-- syntax (Wylie spells it with a letter, the glyphs write nothing) and has no
-- token, so 'Ast.stackTokens' already leaves it out of the comparison.
shapesW :: T.Text -> Either T.Text [(Ast.Role, [TokenCanonical])]
shapesW = fmap (map shape . concatMap stacksOf) . parseRuns Wylie

shapesU :: T.Text -> Either T.Text [(Ast.Role, [TokenCanonical])]
shapesU = fmap (map shape . concatMap stacksOf) . parseRuns Tibetan

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
stacksW = fmap (concatMap stacksOf) . parseRuns Wylie

stacksU :: T.Text -> Either T.Text [Ast.Stack]
stacksU = fmap (concatMap stacksOf) . parseRuns Tibetan

rawsOfStack :: Ast.Stack -> [T.Text]
rawsOfStack = map tokenRaw . toList . Ast.stackTokens

tokensOf :: Syllable -> [Token]
tokensOf = toList . Ast.flatten . sylStacks

stacksOf :: Syllable -> [Ast.Stack]
stacksOf = toList . sylStacks

parseRuns :: Spelling -> T.Text -> Either T.Text [Syllable]
parseRuns spelling input =
    fmap
        ( \items ->
            [s | SyllableItem s <- items]
        )
        (parseEither (pSentence spelling) (fst (tokenize input)))
    where
        tokenize = case spelling of
            Wylie -> tokenizeWylie
            Tibetan -> tokenizeUnicode

-- | Read a vector file as UTF-8 regardless of the locale, so the corpora decode
-- identically everywhere. Same helper as in Test.Parity, kept local.
readUtf8File :: FilePath -> IO T.Text
readUtf8File path = do
    bytes <- BS.readFile path
    case TE.decodeUtf8' bytes of
        Left err -> error (path ++ ": not valid UTF-8: " ++ show err)
        Right text -> pure text
