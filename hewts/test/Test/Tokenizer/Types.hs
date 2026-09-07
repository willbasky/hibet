module Test.Tokenizer.Types (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

tests :: TestTree
tests =
    testGroup
        "types"
        [ testGroup "wylie" (map mkCase wylieCases)
        , testGroup "unicode" (map mkCase unicodeCases)
        ]

mkCase :: (String, Assertion) -> TestTree
mkCase (name, assertion) = testCase name assertion

singleWylie :: Text -> (TokenKind, TokenCanonical) -> Assertion
singleWylie input expected =
    case tokenizeWylie input of
        [tok] -> (tokenKind tok, tokenCanonical tok) @?= expected
        xs -> error $ "Expected 1 token, got " <> show (length xs) <> " for input: " <> show input

singleUnicode :: Text -> (TokenKind, TokenCanonical) -> Assertion
singleUnicode input expected =
    case tokenizeUnicode input of
        [tok] -> (tokenKind tok, tokenCanonical tok) @?= expected
        xs -> error $ "Expected 1 token, got " <> show (length xs) <> " for input: " <> show input

wylieCases :: [(String, Assertion)]
wylieCases =
    [ ("consonant basic k", singleWylie "k" (TkConsonant, TcConsonant Ck))
    , ("consonant aspirated kh", singleWylie "kh" (TkConsonant, TcConsonant Ckh))
    , ("consonant plus-form gh -> CgPLUSh", singleWylie "gh" (TkConsonant, TcConsonant CgPLUSh))
    , ("consonant explicit plus g+h", singleWylie "g+h" (TkConsonant, TcConsonant CgPLUSh))
    , ("consonant alias W -> Cw", singleWylie "W" (TkConsonant, TcConsonant Cw))
    , ("consonant stacked dz+h", singleWylie "dz+h" (TkConsonant, TcConsonant CdzPLUSh))
    , ("vowel short i", singleWylie "i" (TkVowel, TcVowel Vi))
    , ("vowel long alias O -> Vo", singleWylie "O" (TkVowel, TcVowel Vo))
    , ("vowel composite au", singleWylie "au" (TkVowel, TcVowel Vau))
    , ("vowel minus form -I", singleWylie "-I" (TkVowel, TcVowel V_I))
    , ("final variant ~M` -> FMAnusvara", singleWylie "~M`" (TkFinal, TcFinal FMAnusvara))
    , ("final nasal variant ~X", singleWylie "~X" (TkFinal, TcFinal FMCandrabinduOrNasal))
    , ("final visarga H", singleWylie "H" (TkFinal, TcFinal FMVisarga))
    , ("number 0", singleWylie "0" (TkNumber, TcNumber N0))
    , ("number 9", singleWylie "9" (TkNumber, TcNumber N9))
    , ("punctuation tsheg space", singleWylie " " (TkPunctuation, TcPunctuation PMTsheg))
    , ("punctuation double shad", singleWylie "//" (TkPunctuation, TcPunctuation PMNyisShad))
    , ("punctuation gter tshig mgo", singleWylie ":" (TkPunctuation, TcPunctuation PMGterTshigMgo))
    , ("symbol exclamation", singleWylie "!" (TkSymbol, TcSymbol SMExclamation))
    , ("symbol right paren", singleWylie ")" (TkSymbol, TcSymbol SMRParen))
    , ("explicit space marker underscore", singleWylie "_" (TkSpace, TcSpace SMSpace))
    , ("special marker out-of-context becomes unknown", singleWylie "+" (TkUnknown, TcUnknown (UnknownMark "+")))
    , ("unknown latin x becomes unknown", singleWylie "x" (TkUnknown, TcUnknown (UnknownMark "x")))
    ]

unicodeCases :: [(String, Assertion)]
unicodeCases =
    [ ("consonant basic ka", singleUnicode "ཀ" (TkConsonant, TcConsonant Ck))
    , ("consonant plus-form ga+ha", singleUnicode "གྷ" (TkConsonant, TcConsonant CgPLUSh))
    , ("consonant stacked-ksha", singleUnicode "ཀྵ" (TkConsonant, TcConsonant CkPLUSSh))
    , ("subconsonant ya", singleUnicode "ྱ" (TkSubConsonant, TcSubConsonant SCy))
    , ("subconsonant R", singleUnicode "ྼ" (TkSubConsonant, TcSubConsonant SCR))
    , ("vowel i", singleUnicode "ི" (TkVowel, TcVowel Vi))
    , ("vowel au", singleUnicode "ཽ" (TkVowel, TcVowel Vau))
    , ("vowel minus i", singleUnicode "ྀ" (TkVowel, TcVowel V_i))
    , ("final anusvara", singleUnicode "ཾ" (TkFinal, TcFinal FMAnusvara))
    , ("final halanta", singleUnicode "྄" (TkFinal, TcFinal FMHalanta))
    , ("final yig mgo", singleUnicode "྅" (TkFinal, TcFinal FMYigMgo))
    , ("number 1", singleUnicode "༡" (TkNumber, TcNumber N1))
    , ("number 8", singleUnicode "༨" (TkNumber, TcNumber N8))
    , ("punctuation tsheg", singleUnicode "་" (TkPunctuation, TcPunctuation PMTsheg))
    , ("punctuation shad", singleUnicode "།" (TkPunctuation, TcPunctuation PMShad))
    , ("punctuation nyis shad", singleUnicode "༎" (TkPunctuation, TcPunctuation PMNyisShad))
    , ("symbol at-like mark", singleUnicode "༄" (TkSymbol, TcSymbol SMAt))
    , ("symbol opening mark", singleUnicode "༼" (TkSymbol, TcSymbol SMLParen))
    , ("ascii space becomes TkSpace", singleUnicode " " (TkSpace, TcSpace SMSpace))
    , ("unknown latin x becomes unknown", singleUnicode "x" (TkUnknown, TcUnknown (UnknownMark "x")))
    ]
