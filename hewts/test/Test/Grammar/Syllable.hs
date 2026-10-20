-- Ожидания: hewts — мост AST (REFACTOR-AST шаг 1); корпуса — генератор.

module Test.Grammar.Syllable (tests) where

import Convert (SpellItem (..), pSentence)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Grammar.Syllable (Position (..))
import qualified Convert.Grammar.Syllable as Ast
import Convert.Sentence (Syllable (..))
import Convert.Token
    ( Consonant (..)
    , FinalMark (..)
    , SubConsonant (..)
    , Token
    , TokenCanonical (..)
    , Vowel (..)
    , tokenCanonical
    , tokenRaw
    , tokenSpan
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Sequence as Seq
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
            fmap (map stackRaws) (stacksW "rka") @?= Right [["r", "k"]]
        , testCase "kab: the suffix opens its own stack" $ do
            fmap (map Ast.stRole) (stacksW "kab")
                @?= Right [Ast.RoleRoot, Ast.RoleSuffix]
            fmap (map stackRaws) (stacksW "kab") @?= Right [["k"], ["b"]]
        , testCase "kabs: the postfix opens its own stack" $ do
            fmap (map Ast.stRole) (stacksW "kabs")
                @?= Right [Ast.RoleRoot, Ast.RoleSuffix, Ast.RolePostfix]
            fmap (map stackRaws) (stacksW "kabs")
                @?= Right [["k"], ["b"], ["s"]]
        , testCase "བཾྲ: canonical order is root, subfix, final" $ do
            canonicalsU "བཾྲ"
                @?= Right [TcConsonant Cb, TcSubConsonant SCr, TcFinal FMAnusvara]
            fmap (map stackRaws) (stacksU "བཾྲ")
                @?= Right [["བ", "ྲ", "ཾ"]]
            fmap
                (map (map tokenRaw . Ast.finalsTokens . Ast.stFinals))
                (stacksU "བཾྲ")
                @?= Right [["ཾ"]]
        , testCase "f: the caret is its own field" $
            canonicalsW "f" @?= Right [TcConsonant Cph, TcFinal FMCaret]
        , testCase "ཧྥ: the subjoined root stays in the base stack" $ do
            fmap (map Ast.stRole) (stacksU "ཧྥ") @?= Right [Ast.RoleRoot]
            fmap (map stackRaws) (stacksU "ཧྥ")
                @?= Right [["ཧ", "ྥ"]]
            fmap (map stackRaws) (stacksU "ཧྥི")
                @?= Right [["ཧ", "ྥ", "ི"]]
        , testCase "bo+ra: the a beside a written vowel is input syntax" $
            canonicalsW "bo+ra" @?= Right [TcConsonant Cb, TcConsonant Cr, TcVowel Vo]
        , testCase "bru+e: both written vowels stay in one slot" $
            canonicalsW "bru+e"
                @?= Right [TcConsonant Cb, TcConsonant Cr, TcVowel Vu, TcVowel Ve]
        , testCase "g+m+r+a: three subjoined letters stay in the stack" $
            canonicalsW "g+m+r+a"
                @?= Right [TcConsonant Cg, TcConsonant Cm, TcConsonant Cr, TcConsonant Ca]
        , testCase "g.yul: today three roots (REFACTOR-AST §8 A2)" $ do
            rolesW "g.yul"
                @?= Right [Ast.RoleRoot, Ast.RoleRoot, Ast.RoleRoot]
            canonicalsW "g.yul"
                @?= Right [TcConsonant Cg, TcConsonant Cy, TcVowel Vu, TcConsonant Cl]
        , testCase "corpora: kept tokens are in stacks, sorted flat = run" checkCorpora
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

-- | Every run of both spellings: the tokens the grammar gave a place must all
-- come back from the stacks, and sorting the flat stacks by source span must
-- give the run back. Eaten marks, separators and boundaries are not in the
-- stacks; the implicit vowel is a fact, not a token (see 'kept').
--
-- `སྒྲྭ` is in the corpora, but the generic parse still loses its superfix
-- (REFACTOR-AST §8 A3), so its stack shape is not asserted here yet.
checkCorpora :: Assertion
checkCorpora = do
    wylie <- concat <$> mapM (readRuns Wylie wylieColumn) wylieFiles
    kangyur <- readRuns Tibetan id "test/vectors/kang.txt"
    unicodeVectors <- concat <$> mapM (readRuns Tibetan unicodeColumn) wylieFiles
    checkSide "Wylie" wylie
    checkSide "Unicode" (kangyur <> unicodeVectors)
    where
        checkSide side runs = do
            let lost =
                    [ (run, kept run, toList (Ast.flatten (Ast.stacksOf (Seq.fromList run))))
                    | run <- runs
                    , let stored = kept run
                          flat = toList (Ast.flatten (Ast.stacksOf (Seq.fromList run)))
                    , sortOn tokenSpan flat /= sortOn tokenSpan stored
                    ]
            assertBool
                (side <> ": tokens lost by the bridge:\n" <> unlines (map show lost))
                (null lost)

-- | The tokens a stack is supposed to keep: everything the grammar marked with
-- a place except the eaten marks and the implicit vowel. The implicit @a@ is a
-- fact of the stack, not a sign (REFACTOR-AST §4, 'Ast.VowelImplicit'), so it
-- has no token to flatten.
kept :: [(Maybe Position, Token)] -> [Token]
kept run =
    [ t
    | (pos, t) <- run
    , pos `notElem` [Nothing, Just Eaten, Just ImplicitVowel]
    ]

wylieFiles :: [FilePath]
wylieFiles =
    [ "test/vectors/jsewts_parity.tsv"
    , "test/vectors/lingua_parity.tsv"
    , "test/vectors/ewts_js_parity.tsv"
    , "test/vectors/ewts_rs_rules.tsv"
    , "test/vectors/java_ewts.tsv"
    ]

readRuns ::
    Spelling
    -> (T.Text -> T.Text)
    -> FilePath
    -> IO [[(Maybe Position, Token)]]
readRuns spelling column path = do
    text <- readUtf8File path
    pure
        [ run
        | line <- T.lines text
        , let clean = T.dropWhile (== '\xfeff') line
        , not (T.null clean)
        , not ("#" `T.isPrefixOf` clean)
        , Right items <-
            [parseEither (pSentence spelling) (fst (tokenize (column clean)))]
        , SyllableItem s <- items
        , let run = toList (syllableTokens s)
        , not (null run)
        ]
    where
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
rolesW = fmap (concatMap (map Ast.stRole . stacksOf)) . parseRuns Wylie

stacksW :: T.Text -> Either T.Text [Ast.Stack]
stacksW = fmap (concatMap stacksOf) . parseRuns Wylie

stacksU :: T.Text -> Either T.Text [Ast.Stack]
stacksU = fmap (concatMap stacksOf) . parseRuns Tibetan

stackRaws :: Ast.Stack -> [T.Text]
stackRaws = map tokenRaw . toList . Ast.stackTokens

tokensOf :: [(Maybe Position, Token)] -> [Token]
tokensOf = toList . Ast.flatten . Ast.stacksOf . Seq.fromList

stacksOf :: [(Maybe Position, Token)] -> [Ast.Stack]
stacksOf = toList . Ast.stacksOf . Seq.fromList

parseRuns :: Spelling -> T.Text -> Either T.Text [[(Maybe Position, Token)]]
parseRuns spelling input =
    fmap
        ( \items ->
            [ toList (syllableTokens s)
            | SyllableItem s <- items
            ]
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
