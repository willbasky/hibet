module Test.Tokenizer.Tricky (tests) where

import Convert.Token
import Convert.Tokenizer.Wylie
    ( consonantTokenMap
    , finalTokenMap
    , longTokenList
    , numberTokenMap
    , punctuationTokenMap
    , symbolTokenMap
    , tokenizeWylie
    , vowelTokenMap
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "tricky"
        [ testCase "wylie longest-match dzh stays one token" caseWylieDzh
        , testCase "wylie longest-match -d+h beats -d" caseWylieDashDH
        , testCase "wylie CRLF is a single lexical chunk" caseWylieCRLFChunk
        , testCase "wylie bracketed non-tibetan block is a single token" caseWylieBracketedChunk
        , testCase "wylie unicode escape chunk is a single token" caseWylieEscapeChunk
        , testCase "wylie special marker carries InvalidSequence warning" caseWylieSpecialIssue
        , testCase "wylie longest-match k+Sh is single chunk" caseWylieKPlusShChunk
        , testCase "wylie longest-match dz+h then a" caseWylieDzPlusHThenA
        , testCase "wylie longest-match -d+h then a" caseWylieDashDPlusHThenA
        , testCase "wylie longest-match g+h then o" caseWylieGPlusHThenO
        , testCase "wylie longest-match ~M` then a" caseWylieAnusvaraThenA
        , testCase "wylie longest-match -I then a" caseWylieMinusIThenA
        , testCase "wylie longest-match ai then a" caseWylieAiThenA
        , testCase "wylie longest-match // then a" caseWylieDoubleShadThenA
        , testCase "wylie longest-match Sh then a" caseWylieShThenA
        , testCase "wylie longest-match th then a" caseWylieThThenA
        , testGroup "wylie prefix conflicts" (map mkPrefixCase wyliePrefixCases)
        , testCase "wylie mixed special markers keep InvalidSequence diagnostics" caseWylieMixedSpecialDiagnostics
        , testCase "wylie unknown latin keeps UnknownChar diagnostics" caseWylieUnknownDiagnostics
        , testCase "unicode unexpected ASCII between Tibetan signs is isolated" caseUnicodeUnexpectedAsciiBetweenTibetan
        , testCase "unicode repeated vowel marks are tokenized separately" caseUnicodeRepeatedVowels
        , testCase "unicode rare combining mark is unknown with warning" caseUnicodeRareCombiningUnknown
        , testCase "unicode mixed edge stream preserves per-token diagnostics" caseUnicodeMixedEdgeDiagnostics
        , testCase "unicode tsheg and ASCII space are different kinds" caseUnicodeTshegVsSpace
        , testCase "unicode unknown ASCII is preserved" caseUnicodeUnknownPreserved
        , testGroup "wylie chunking contracts" wylieChunkingContractTests
        , testGroup "unicode normalization matrix" (map mkUnicodeNormalizationCase unicodeNormalizationCases)
        , testCase "unicode precomposed U+0F73 maps to VI" caseUnicodePrecomposed073
        , testCase "unicode decomposed U+0F71 U+0F72 maps to VA+Vi" caseUnicodeDecomposed071072
        , testCase "unicode precomposed U+0F75 maps to VU" caseUnicodePrecomposed075
        , testCase "unicode decomposed U+0F71 U+0F74 maps to VA+Vu" caseUnicodeDecomposed071074
        ]

mkPrefixCase :: (String, Text, [Text]) -> TestTree
mkPrefixCase (name, input, expected) =
    testCase name (assertWylieRawTokens input expected)

wyliePrefixCases :: [(String, Text, [Text])]
wyliePrefixCases =
    [ ("plain d before vowel", "da", ["d", "a"])
    , ("dz beats d", "dza", ["dz", "a"])
    , ("dzh beats dz", "dzha", ["dzh", "a"])
    , ("dz+h beats dzh", "dz+ha", ["dz+h", "a"])
    , ("plain -d before vowel", "-da", ["-d", "a"])
    , ("-dh beats -d", "-dha", ["-dh", "a"])
    , ("-d+h beats -dh", "-d+ha", ["-d+h", "a"])
    , ("g+h beats g", "g+ha", ["g+h", "a"])
    , ("gh alias stays one chunk", "gha", ["gh", "a"])
    , ("D+h beats D", "D+ha", ["D+h", "a"])
    , ("Dh alias stays one chunk", "Dha", ["Dh", "a"])
    , ("b+h beats b", "b+ha", ["b+h", "a"])
    , ("bh alias stays one chunk", "bha", ["bh", "a"])
    , ("th beats t", "tha", ["th", "a"])
    , ("tsh beats th", "tsha", ["tsh", "a"])
    , ("sh beats s", "sha", ["sh", "a"])
    , ("Sh beats s", "Sha", ["Sh", "a"])
    , ("-sh beats -s fallback", "-sha", ["-sh", "a"])
    , ("// beats /", "//a", ["//", "a"])
    , ("single slash remains /", "/a", ["/", "a"])
    , ("ai beats a", "aia", ["ai", "a"])
    , ("au beats a", "aua", ["au", "a"])
    , ("-I beats - and I", "-Ia", ["-I", "a"])
    , ("~M` beats ~M", "~M`a", ["~M`", "a"])
    , ("~M works standalone", "~Ma", ["~M", "a"])
    , ("~X works standalone", "~Xa", ["~X", "a"])
    , ("k+Sh beats k", "k+Sha", ["k+Sh", "a"])
    , ("CRLF beats CR", "\r\na", ["\r\n", "a"])
    ]

caseWylieDzh :: Assertion
caseWylieDzh =
    case tokenizeWylie "dzh" of
        [tok] -> (tokenKind tok, tokenCanonical tok) @?= (TkConsonant, TcConsonant CdzPLUSh)
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieDashDH :: Assertion
caseWylieDashDH =
    case tokenizeWylie "-d+h" of
        [tok] -> tokenCanonical tok @?= TcConsonant CDPLUSh
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieCRLFChunk :: Assertion
caseWylieCRLFChunk =
    case tokenizeWylie "\r\n" of
        [tok] -> tokenRaw tok @?= "\r\n"
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieBracketedChunk :: Assertion
caseWylieBracketedChunk =
    case tokenizeWylie "[ab[cd]e]k" of
        [blockTok, kTok] -> do
            tokenRaw blockTok @?= "[ab[cd]e]"
            tokenIssues blockTok @?= []
            tokenRaw kTok @?= "k"
        xs -> error $ "Expected 2 tokens, got " <> show (length xs)

caseWylieEscapeChunk :: Assertion
caseWylieEscapeChunk =
    case tokenizeWylie "\\u0f40a" of
        [escTok, aTok] -> do
            tokenRaw escTok @?= "\\u0f40"
            tokenIssues escTok @?= []
            tokenRaw aTok @?= "a"
        xs -> error $ "Expected 2 tokens, got " <> show (length xs)

caseWylieSpecialIssue :: Assertion
caseWylieSpecialIssue =
    case tokenizeWylie "~" of
        [tok] -> tokenIssues tok @?= [TokenIssue InvalidSequence TisWarning "Special marker out of context"]
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieKPlusShChunk :: Assertion
caseWylieKPlusShChunk =
    assertWylieRawTokens "k+Sh" ["k+Sh"]

caseWylieDzPlusHThenA :: Assertion
caseWylieDzPlusHThenA =
    assertWylieRawTokens "dz+ha" ["dz+h", "a"]

caseWylieDashDPlusHThenA :: Assertion
caseWylieDashDPlusHThenA =
    assertWylieRawTokens "-d+ha" ["-d+h", "a"]

caseWylieGPlusHThenO :: Assertion
caseWylieGPlusHThenO =
    assertWylieRawTokens "g+ho" ["g+h", "o"]

caseWylieAnusvaraThenA :: Assertion
caseWylieAnusvaraThenA =
    assertWylieRawTokens "~M`a" ["~M`", "a"]

caseWylieMinusIThenA :: Assertion
caseWylieMinusIThenA =
    assertWylieRawTokens "-Ia" ["-I", "a"]

caseWylieAiThenA :: Assertion
caseWylieAiThenA =
    assertWylieRawTokens "aia" ["ai", "a"]

caseWylieDoubleShadThenA :: Assertion
caseWylieDoubleShadThenA =
    assertWylieRawTokens "//a" ["//", "a"]

caseWylieShThenA :: Assertion
caseWylieShThenA =
    assertWylieRawTokens "Sha" ["Sh", "a"]

caseWylieThThenA :: Assertion
caseWylieThThenA =
    assertWylieRawTokens "tha" ["th", "a"]

caseWylieMixedSpecialDiagnostics :: Assertion
caseWylieMixedSpecialDiagnostics =
    let toks = tokenizeWylie "~+`]-."
        expectedIssue = [TokenIssue InvalidSequence TisWarning "Special marker out of context"]
     in do
            map tokenRaw toks @?= ["~", "+", "`", "]", "-", "."]
            map tokenKind toks @?= replicate 6 TkUnknown
            map tokenCanonical toks
                @?= map (TcUnknown . UnknownMark) ["~", "+", "`", "]", "-", "."]
            map tokenIssues toks @?= replicate 6 expectedIssue

caseWylieUnknownDiagnostics :: Assertion
caseWylieUnknownDiagnostics =
    case tokenizeWylie "x" of
        [tok] -> tokenIssues tok @?= [TokenIssue UnknownChar TisWarning "Unknown token"]
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseUnicodeUnexpectedAsciiBetweenTibetan :: Assertion
caseUnicodeUnexpectedAsciiBetweenTibetan =
    case tokenizeUnicode "ཀxི" of
        [kTok, xTok, iTok] -> do
            tokenKind kTok @?= TkConsonant
            tokenKind xTok @?= TkUnknown
            tokenKind iTok @?= TkVowel
            tokenCanonical xTok @?= TcUnknown (UnknownMark "x")
            tokenIssues xTok @?= [TokenIssue UnknownChar TisWarning "Unknown token"]
        xs -> error $ "Expected 3 tokens, got " <> show (length xs)

caseUnicodeRepeatedVowels :: Assertion
caseUnicodeRepeatedVowels =
    case tokenizeUnicode "ཀིི" of
        [kTok, i1Tok, i2Tok] -> do
            tokenKind kTok @?= TkConsonant
            tokenKind i1Tok @?= TkVowel
            tokenKind i2Tok @?= TkVowel
            tokenCanonical i1Tok @?= TcVowel Vi
            tokenCanonical i2Tok @?= TcVowel Vi
            tokenIssues i1Tok @?= []
            tokenIssues i2Tok @?= []
        xs -> error $ "Expected 3 tokens, got " <> show (length xs)

caseUnicodeRareCombiningUnknown :: Assertion
caseUnicodeRareCombiningUnknown =
    case tokenizeUnicode "྆" of
        [tok] -> do
            tokenKind tok @?= TkUnknown
            tokenCanonical tok @?= TcUnknown (UnknownMark "྆")
            tokenIssues tok @?= [TokenIssue UnknownChar TisWarning "Unknown token"]
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseUnicodeMixedEdgeDiagnostics :: Assertion
caseUnicodeMixedEdgeDiagnostics =
    let toks = tokenizeUnicode "ཀིི ཀxི ྆།"
        issuesByRaw = map (\tok -> (tokenRaw tok, tokenIssues tok)) toks
     in issuesByRaw
            @?=
                [ ("ཀ", [])
                , ("ི", [])
                , ("ི", [])
                , (" ", [])
                , ("ཀ", [])
                , ("x", [TokenIssue UnknownChar TisWarning "Unknown token"])
                , ("ི", [])
                , (" ", [])
                , ("྆", [TokenIssue UnknownChar TisWarning "Unknown token"])
                , ("།", [])
                ]

caseUnicodeTshegVsSpace :: Assertion
caseUnicodeTshegVsSpace =
    case tokenizeUnicode "་ " of
        [tshegTok, spaceTok] -> do
            tokenKind tshegTok @?= TkPunctuation
            tokenKind spaceTok @?= TkSpace
        xs -> error $ "Expected 2 tokens, got " <> show (length xs)

caseUnicodeUnknownPreserved :: Assertion
caseUnicodeUnknownPreserved =
    case tokenizeUnicode "x" of
        [tok] -> do
            tokenCanonical tok @?= TcUnknown (UnknownMark "x")
            tokenIssues tok @?= [TokenIssue UnknownChar TisWarning "Unknown token"]
        xs -> error $ "Expected 1 token, got " <> show (length xs)

assertWylieRawTokens :: Text -> [Text] -> Assertion
assertWylieRawTokens input expected =
    (tokenRaw <$> tokenizeWylie input) @?= expected

wylieChunkingContractTests :: [TestTree]
wylieChunkingContractTests =
    [ testCase "all multi-char tokenizer keys are listed in longTokenList" caseAllMultiCharKeysListed
    , testCase "no dead multi-char tokenizer keys" caseNoDeadMultiCharKeys
    ]

caseAllMultiCharKeysListed :: Assertion
caseAllMultiCharKeysListed =
    let longTokens = HS.fromList longTokenList
        missing = filter (not . (`HS.member` longTokens)) multiCharTokenizerKeys
     in missing @?= []

caseNoDeadMultiCharKeys :: Assertion
caseNoDeadMultiCharKeys =
    mapM_ assertReachableMultiCharKey multiCharTokenizerKeys

assertReachableMultiCharKey :: Text -> Assertion
assertReachableMultiCharKey key =
    case tokenizeWylie key of
        [tok] -> tokenRaw tok @?= key
        toks -> error $ "Expected a single token for key " <> show key <> ", got: " <> show (map tokenRaw toks)

multiCharTokenizerKeys :: [Text]
multiCharTokenizerKeys =
    HS.toList . HS.fromList . filter ((> 1) . T.length) $
        HM.keys consonantTokenMap
            <> HM.keys vowelTokenMap
            <> HM.keys finalTokenMap
            <> HM.keys numberTokenMap
            <> HM.keys punctuationTokenMap
            <> HM.keys symbolTokenMap

mkUnicodeNormalizationCase :: (String, Text, [Text]) -> TestTree
mkUnicodeNormalizationCase (name, input, expectedUnknownRaws) =
    testCase name (assertUnicodeNormalizationCase input expectedUnknownRaws)

assertUnicodeNormalizationCase :: Text -> [Text] -> Assertion
assertUnicodeNormalizationCase input expectedUnknownRaws = do
    let toks = tokenizeUnicode input
        raws = map tokenRaw toks
        unknowns = [tok | tok <- toks, tokenKind tok == TkUnknown]
    assertBool "Unicode stream contains empty tokenRaw" (all (not . T.null) raws)
    T.concat raws @?= input
    map tokenRaw unknowns @?= expectedUnknownRaws
    mapM_ assertUnknownShape unknowns

assertUnknownShape :: Token -> Assertion
assertUnknownShape tok = do
    tokenCanonical tok @?= TcUnknown (UnknownMark (tokenRaw tok))
    tokenIssues tok @?= [TokenIssue UnknownChar TisWarning "Unknown token"]

unicodeNormalizationCases :: [(String, Text, [Text])]
unicodeNormalizationCases =
    [ ("precomposed U+0F73", "ཱི", [])
    , ("decomposed U+0F71 U+0F72", "ཱི", [])
    , ("precomposed U+0F75", "ཱུ", [])
    , ("decomposed U+0F71 U+0F74", "ཱུ", [])
    , ("precomposed U+0F77 unknown", "ཷ", ["ཷ"])
    , ("decomposed for U+0F77", "ཱྀུ", [])
    , ("precomposed U+0F79 unknown", "ཹ", ["ཹ"])
    , ("decomposed for U+0F79", "ཱྀ", [])
    , ("syllable with precomposed U+0F73", "ཁཱི", [])
    , ("syllable with decomposed U+0F71 U+0F72", "ཁཱི", [])
    , ("repeated vowel i", "ཀིི", [])
    , ("repeated vowel u", "ཀུུ", [])
    , ("repeated vowel e", "ཀེེ", [])
    , ("repeated vowel o", "ཀོོ", [])
    , ("repeated vowel A", "ཀཱཱ", [])
    , ("repeated minus-i vowel", "ཀྀྀ", [])
    , ("repeated anusvara", "ཀཾཾ", [])
    , ("repeated halanta", "ཀ྄྄", [])
    , ("double shad punctuation", "ཀ།།", [])
    , ("double tsheg punctuation", "ཀ་་", [])
    , ("ascii letter between Tibetan chars", "ཀxི", ["x"])
    , ("ascii question between Tibetan chars", "ཀ?ི", ["?"])
    , ("ascii at between Tibetan chars", "ཀ@ི", ["@"])
    , ("ascii space between Tibetan chars", "ཀ ི", [])
    , ("tab between Tibetan chars", "ཀ\tི", ["\t"])
    , ("newline between Tibetan chars", "ཀ\nི", ["\n"])
    , ("carriage return between Tibetan chars", "ཀ\rི", ["\r"])
    , ("ascii hash between Tibetan chars", "ཀ#ི", ["#"])
    , ("ascii digit between Tibetan chars", "ཀ0ི", ["0"])
    , ("leading NUL control", "\NULཀ", ["\NUL"])
    , ("NUL inside Tibetan chars", "ཀ\NULི", ["\NUL"])
    , ("DEL control between Tibetan chars", "ཀ\DELི", ["\DEL"])
    , ("rare combining mark inside syllable", "ཀི྆", ["྆"])
    , ("known unicode symbols around unknown ascii", "ཀ༄x།", ["x"])
    , ("unicode sign sequence only", "༄༅༆", [])
    , ("plus in unicode stream", "ཀ+་ི", ["+"])
    , ("newline between shad marks", "ཀ།\n།ི", ["\n"])
    , ("tab between shad marks", "ཀ།\t།ི", ["\t"])
    , ("carriage return between shad marks", "ཀ།\r།ི", ["\r"])
    , ("NUL between shad marks", "ཀ།\NUL།ི", ["\NUL"])
    ]

caseUnicodePrecomposed073 :: Assertion
caseUnicodePrecomposed073 =
    map tokenCanonical (tokenizeUnicode "ཱི") @?= [TcVowel VI]

caseUnicodeDecomposed071072 :: Assertion
caseUnicodeDecomposed071072 =
    map tokenCanonical (tokenizeUnicode "ཱི") @?= [TcVowel VA, TcVowel Vi]

caseUnicodePrecomposed075 :: Assertion
caseUnicodePrecomposed075 =
    map tokenCanonical (tokenizeUnicode "ཱུ") @?= [TcVowel VU]

caseUnicodeDecomposed071074 :: Assertion
caseUnicodeDecomposed071074 =
    map tokenCanonical (tokenizeUnicode "ཱུ") @?= [TcVowel VA, TcVowel Vu]
