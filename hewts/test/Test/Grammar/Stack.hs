module Test.Grammar.Stack (tests) where

import Convert (OutputFormat (..), SpellItem (..), renderItems)
import Convert.Grammar.Parser (Parser, Spelling (..), parseEither)
import Convert.Grammar.Stack (pStack)
import Convert.Grammar.Word (Position (..), TibetanWord)
import Convert.Token (Token)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | The generic stack parses the Wylie spellings the 37 book structures cannot
-- name and marks them the way jsewts does: the forced stacks (@+@), the caret,
-- the vowel-only escapes, the stand-alone subjoined letters. The plain
-- spellings the book grammar does name (kya, sgrwa, rka, bsgribs, rta, ug, ...)
-- are covered by the Wylie arms of the structures in Test.Grammar.Wylie; here
-- only the forms that still need the generic stack until the wave-3.5
-- wylie-only handling (the caret, the plus, the vowel letters) stays.
tests :: TestTree
tests =
    testGroup
        "grammar stack (generic)"
        [ testCase "sat+t+wa keeps the base t before the subjoined ones" $
            wylPositions "sat+t+wa"
                @?= Right [Root, ImplicitVowel, Root, Subfix, Subfix, ImplicitVowel]
        , testCase "sat+t+wa renders སཏྟྭ" $
            wylRender "sat+t+wa" @?= Right "སཏྟྭ"
        , testCase "g+m+r+a forces three subjoins" $
            wylPositions "g+m+r+a" @?= Right [Root, Subfix, Subfix, Subfix]
        , testCase "g+m+r+a renders གྨྲྸ" $
            wylRender "g+m+r+a" @?= Right "གྨྲྸ"
        , testCase "g+mra subjoins the bare letter after a forced one" $
            wylRender "g+mra" @?= Right "གྨྲ"
        , testCase "s+ha renders the aspirate སྷ" $
            wylRender "s+ha" @?= Right "སྷ"
        , testCase "s+a renders the subjoined a སྸ" $
            wylRender "s+a" @?= Right "སྸ"
        , testCase "R+na renders ཪྣ" $
            wylRender "R+na" @?= Right "ཪྣ"
        , testCase "R+Ya keeps the raised Y ཪྻ" $
            wylRender "R+Ya" @?= Right "ཪྻ"
        , testCase "R+ya renders ཪྱ" $
            wylRender "R+ya" @?= Right "ཪྱ"
        , testCase "bru+e renders two vowels བྲེུ" $
            wylRender "bru+e" @?= Right "བྲེུ"
        , testCase "ge+a renders གེྸ" $
            wylRender "ge+a" @?= Right "གེྸ"
        , testCase "ba+a renders བྸ" $
            wylRender "ba+a" @?= Right "བྸ"
        , testCase "a+a renders ཨྸ" $
            wylRender "a+a" @?= Right "ཨྸ"
        , testCase "b+ae renders བྸེ" $
            wylRender "b+ae" @?= Right "བྸེ"
        , testCase "ph^a keeps the caret at the end" $
            wylRender "ph^a" @?= Right "ཕ༹"
        , testCase "g^ra renders གྲ༹" $
            wylRender "g^ra" @?= Right "གྲ༹"
        , testCase "gr^a renders གྲ༹" $
            wylRender "gr^a" @?= Right "གྲ༹"
        , testCase "gra^ renders གྲ༹" $
            wylRender "gra^" @?= Right "གྲ༹"
        , testCase "g^r^a collapses the two carets to one" $
            wylRender "g^r^a" @?= Right "གྲ༹"
        , testCase "f+ra keeps the f caret in place" $
            wylRender "f+ra" @?= Right "ཕ༹ྲ"
        , testCase "v+la keeps the v caret in place" $
            wylRender "v+la" @?= Right "བ༹ླ"
        , testCase "a+yo renders ཨྱོ" $
            wylRender "a+yo" @?= Right "ཨྱོ"
        , testCase "a stack cannot start on a subjoined letter (r-i stays Other)" $
            isLeft (wylParse (pStack Wylie) "r-i") @?= True
        , testCase "a stack cannot start on a plus" $
            isLeft (wylParse (pStack Wylie) "+a") @?= True
        ]

wylParse :: Parser TibetanWord -> Text -> Either Text TibetanWord
wylParse p input = parseEither p (fst (tokenizeWylie input))

wylPositions :: Text -> Either Text [Position]
wylPositions = fmap (toList . fmap fst) . wylParse (pStack Wylie)

wylRender :: Text -> Either Text Text
wylRender w = fmap (renderItems OutUnicode . pure . Syllable) (wylParse (pStack Wylie) w)

-- | The tokens of a Wylie spelling, for the tests that want to see them.
_wylTokens :: Text -> [Token]
_wylTokens = fst . tokenizeWylie
