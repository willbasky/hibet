-- | The Wylie arms of the grammar, walked from the Wylie input.
--
-- Parsing Wylie goes through the same 37 structures as Tibetan, but with the
-- spelling-dispatching arms: the implicit @a@ is written and consumed as the
-- vowel ('ImplicitVowel'), a superfix is a full letter over a root, and so on.
-- These tests pin the standard spellings that used to reach only the generic
-- stack to their new home in the arms, so the coverage survives the stack's
-- retirement. Every case here runs @pSentence Wylie@ (the w2u direction) and
-- expects exactly one syllable whose marks and Unicode match the reference.
--
-- Where the arms disagree with the old stack, they do so on purpose: bsgribs
-- reads the trailing བ / ས as Suffix and Postfix (the book reading), not as
-- two bare roots, and the chen-ma spellings (dang, 'ang) carry the implicit @a@.
module Test.Grammar.Wylie (tests) where

import Convert (OutputFormat (..), SpellItem (..), renderItems)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Grammar.Word (Position (..))
import Convert.Sentence (pSentence)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar wylie arms"
        [ probed "kya -> root + subfix y" "kya" [Root, Subfix, ImplicitVowel] "ཀྱ"
        , probed
            "sgrwa -> superfix over a stack"
            "sgrwa"
            [Superfix, Root, Subfix, Subfix, ImplicitVowel]
            "སྒྲྭ"
        , probed
            "grla backtracks to two stacks (no subjoined r)"
            "grla"
            [Root, Root, Subfix, ImplicitVowel]
            "གརླ"
        , probed
            "rka -> superfix r before a root"
            "rka"
            [Superfix, Root, ImplicitVowel]
            "རྐ"
        , probed
            "bsgribs -> prefix, superfix, root, subfix, vowel, suffix, postfix"
            "bsgribs"
            [Prefix, Superfix, Root, Subfix, Vowel, Suffix, Postfix]
            "བསྒྲིབས"
        , probed
            "g.yon -> stack break, two roots and a vowel"
            "g.yon"
            [Root, Root, Vowel, Root]
            "གཡོན"
        , renders "rta renders རྟ" "rta" "རྟ"
        , renders "rwa renders རྭ" "rwa" "རྭ"
        , renders "sra renders སྲ" "sra" "སྲ"
        , renders "ug writes the a-chen in front of the vowel" "ug" "ཨུག"
        , renders "oM writes ཨོཾ" "oM" "ཨོཾ"
        , renders "AH writes ཨཱཿ" "AH" "ཨཱཿ"
        , renders "mkhan renders མཁན" "mkhan" "མཁན"
        , renders "gyon renders གྱོན" "gyon" "གྱོན"
        , renders "the subfix scan stops at two letters (mrya)" "mrya" "མྲྱ"
        , renders "rbya splits before b (b is not a subfix letter)" "rbya" "རྦྱ"
        , testCase "C18: hpha is ཧཕ, the aspirate written as a subroot" $
            wylRender "hpha" @?= Right "ཧཕ"
        , testCase "C20: 'ang is one a-chung syllable" $
            wylRender "'ang" @?= Right "འང"
        , testCase "C20: 'oM is one a-chung syllable" $
            wylRender "'oM" @?= Right "འོཾ"
        , testCase "dang keeps the implicit a between d and ng" $
            wylRender "dang" @?= Right "དང"
        , testCase "bla keeps the implicit a under the subjoined la" $
            wylRender "bla" @?= Right "བླ"
        ]

-- | Assert the marks and the Unicode render of a one-word Wylie spelling.
probed :: String -> Text -> [Position] -> Text -> TestTree
probed label input marks render =
    testGroup
        label
        [ testCase "marks" $ wylMarks input @?= Right marks
        , testCase "render" $ wylRender input @?= Right render
        ]

renders :: String -> Text -> Text -> TestTree
renders label input render =
    testCase label $
        wylRender input @?= Right render

wylMarks :: Text -> Either Text [Position]
wylMarks input =
    case parseW input of
        Left err -> Left err
        Right items -> case [w | Syllable w <- items] of
            [w] -> Right (toList (fmap fst w))
            _ -> Left "expected the text to be exactly one syllable"

wylRender :: Text -> Either Text Text
wylRender input =
    case parseW input of
        Left err -> Left err
        Right items -> case items of
            [Syllable _] -> Right (renderItems OutUnicode items)
            _ -> Left "expected the text to be exactly one syllable"

-- | The Wylie input through the tokenizer and the Wylie arm of the grammar.
parseW :: Text -> Either Text [SpellItem]
parseW input = parseEither (pSentence Wylie) (fst (tokenizeWylie input))
