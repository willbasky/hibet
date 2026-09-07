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
        , testGroup "wylie aliases" (map mkCase wylieAliasCases)
        , testGroup "wylie tables" (map mkCase wylieTableCases)
        , testGroup "unicode" (map mkCase unicodeCases)
        , testGroup "unicode aliases" (map mkCase unicodeAliasCases)
        , testGroup "unicode tables" (map mkCase unicodeTableCases)
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
    , ("consonant stack k+Sh", singleWylie "k+Sh" (TkConsonant, TcConsonant CkPLUSSh))
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

wylieAliasCases :: [(String, Assertion)]
wylieAliasCases =
    [ aliasesWylie "g+ha aliases" ["gh", "g+h"] (TkConsonant, TcConsonant CgPLUSh)
    , aliasesWylie "D+ha aliases" ["Dh", "D+h", "-dh", "-d+h"] (TkConsonant, TcConsonant CDPLUSh)
    , aliasesWylie "d+ha aliases" ["dh", "d+h"] (TkConsonant, TcConsonant CdPLUSh)
    , aliasesWylie "b+ha aliases" ["bh", "b+h"] (TkConsonant, TcConsonant CbPLUSh)
    , aliasesWylie "dz+ha aliases" ["dzh", "dz+h"] (TkConsonant, TcConsonant CdzPLUSh)
    , aliasesWylie "retroflex ta aliases" ["T", "-t"] (TkConsonant, TcConsonant CT)
    , aliasesWylie "retroflex tha aliases" ["Th", "-th"] (TkConsonant, TcConsonant CTh)
    , aliasesWylie "retroflex da aliases" ["D", "-d"] (TkConsonant, TcConsonant CD)
    , aliasesWylie "retroflex na aliases" ["N", "-n"] (TkConsonant, TcConsonant CN)
    , aliasesWylie "sha aliases" ["Sh", "-sh"] (TkConsonant, TcConsonant CSh)
    , aliasesWylie "wa aliases" ["w", "W"] (TkConsonant, TcConsonant Cw)
    , aliasesWylie "ya aliases" ["y", "Y"] (TkConsonant, TcConsonant Cy)
    , aliasesWylie "o aliases" ["o", "O"] (TkVowel, TcVowel Vo)
    , aliasesWylie "anusvara aliases" ["M", "~M", "~M`"] (TkFinal, TcFinal FMAnusvara)
    , aliasesWylie "nasal aliases" ["X", "~X"] (TkFinal, TcFinal FMCandrabinduOrNasal)
    ]

wylieTableCases :: [(String, Assertion)]
wylieTableCases =
    [ ("wylie consonants full table", assertWylieSingles wylieConsonants)
    , ("wylie vowels full table", assertWylieSingles wylieVowels)
    , ("wylie finals full table", assertWylieSingles wylieFinals)
    , ("wylie numbers full table", assertWylieSingles wylieNumbers)
    , ("wylie punctuation full table", assertWylieSingles wyliePunctuation)
    , ("wylie symbols full table", assertWylieSingles wylieSymbols)
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

unicodeAliasCases :: [(String, Assertion)]
unicodeAliasCases =
    [ aliasesUnicode "unicode anusvara aliases" ['\x0f7e', '\x0f82', '\x0f83'] (TkFinal, TcFinal FMAnusvara)
    , aliasesUnicode "unicode candrabindu aliases" ['\x0f37', '\x0f35'] (TkFinal, TcFinal FMCandrabinduOrNasal)
    ]

unicodeTableCases :: [(String, Assertion)]
unicodeTableCases =
    [ ("unicode consonants full table", assertUnicodeSingles unicodeConsonants)
    , ("unicode sub-consonants full table", assertUnicodeSingles unicodeSubConsonants)
    , ("unicode vowels full table", assertUnicodeSingles unicodeVowels)
    , ("unicode finals full table", assertUnicodeSingles unicodeFinals)
    , ("unicode numbers full table", assertUnicodeSingles unicodeNumbers)
    , ("unicode punctuation full table", assertUnicodeSingles unicodePunctuation)
    , ("unicode symbols full table", assertUnicodeSingles unicodeSymbols)
    ]

wylieConsonants :: [(Text, (TokenKind, TokenCanonical))]
wylieConsonants =
    [ ("k", (TkConsonant, TcConsonant Ck))
    , ("kh", (TkConsonant, TcConsonant Ckh))
    , ("g", (TkConsonant, TcConsonant Cg))
    , ("gh", (TkConsonant, TcConsonant CgPLUSh))
    , ("g+h", (TkConsonant, TcConsonant CgPLUSh))
    , ("ng", (TkConsonant, TcConsonant Cng))
    , ("c", (TkConsonant, TcConsonant Cc))
    , ("ch", (TkConsonant, TcConsonant Cch))
    , ("j", (TkConsonant, TcConsonant Cj))
    , ("ny", (TkConsonant, TcConsonant Cny))
    , ("T", (TkConsonant, TcConsonant CT))
    , ("-t", (TkConsonant, TcConsonant CT))
    , ("Th", (TkConsonant, TcConsonant CTh))
    , ("-th", (TkConsonant, TcConsonant CTh))
    , ("D", (TkConsonant, TcConsonant CD))
    , ("-d", (TkConsonant, TcConsonant CD))
    , ("Dh", (TkConsonant, TcConsonant CDPLUSh))
    , ("D+h", (TkConsonant, TcConsonant CDPLUSh))
    , ("-dh", (TkConsonant, TcConsonant CDPLUSh))
    , ("-d+h", (TkConsonant, TcConsonant CDPLUSh))
    , ("N", (TkConsonant, TcConsonant CN))
    , ("-n", (TkConsonant, TcConsonant CN))
    , ("t", (TkConsonant, TcConsonant Ct))
    , ("th", (TkConsonant, TcConsonant Cth))
    , ("d", (TkConsonant, TcConsonant Cd))
    , ("dh", (TkConsonant, TcConsonant CdPLUSh))
    , ("d+h", (TkConsonant, TcConsonant CdPLUSh))
    , ("n", (TkConsonant, TcConsonant Cn))
    , ("p", (TkConsonant, TcConsonant Cp))
    , ("ph", (TkConsonant, TcConsonant Cph))
    , ("b", (TkConsonant, TcConsonant Cb))
    , ("bh", (TkConsonant, TcConsonant CbPLUSh))
    , ("b+h", (TkConsonant, TcConsonant CbPLUSh))
    , ("m", (TkConsonant, TcConsonant Cm))
    , ("ts", (TkConsonant, TcConsonant Cts))
    , ("tsh", (TkConsonant, TcConsonant Ctsh))
    , ("dz", (TkConsonant, TcConsonant Cdz))
    , ("dzh", (TkConsonant, TcConsonant CdzPLUSh))
    , ("dz+h", (TkConsonant, TcConsonant CdzPLUSh))
    , ("w", (TkConsonant, TcConsonant Cw))
    , ("W", (TkConsonant, TcConsonant Cw))
    , ("zh", (TkConsonant, TcConsonant Czh))
    , ("z", (TkConsonant, TcConsonant Cz))
    , ("'", (TkConsonant, TcConsonant C'))
    , ("y", (TkConsonant, TcConsonant Cy))
    , ("Y", (TkConsonant, TcConsonant Cy))
    , ("r", (TkConsonant, TcConsonant Cr))
    , ("l", (TkConsonant, TcConsonant Cl))
    , ("sh", (TkConsonant, TcConsonant Csh))
    , ("Sh", (TkConsonant, TcConsonant CSh))
    , ("-sh", (TkConsonant, TcConsonant CSh))
    , ("s", (TkConsonant, TcConsonant Cs))
    , ("h", (TkConsonant, TcConsonant Ch))
    , ("a", (TkConsonant, TcConsonant Ca))
    , ("k+Sh", (TkConsonant, TcConsonant CkPLUSSh))
    , ("R", (TkConsonant, TcConsonant CR))
    ]

wylieVowels :: [(Text, (TokenKind, TokenCanonical))]
wylieVowels =
    [ ("A", (TkVowel, TcVowel VA))
    , ("i", (TkVowel, TcVowel Vi))
    , ("I", (TkVowel, TcVowel VI))
    , ("u", (TkVowel, TcVowel Vu))
    , ("U", (TkVowel, TcVowel VU))
    , ("e", (TkVowel, TcVowel Ve))
    , ("ai", (TkVowel, TcVowel Vai))
    , ("o", (TkVowel, TcVowel Vo))
    , ("O", (TkVowel, TcVowel Vo))
    , ("au", (TkVowel, TcVowel Vau))
    , ("-i", (TkVowel, TcVowel V_i))
    , ("-I", (TkVowel, TcVowel V_I))
    ]

wylieFinals :: [(Text, (TokenKind, TokenCanonical))]
wylieFinals =
    [ ("M", (TkFinal, TcFinal FMAnusvara))
    , ("~M`", (TkFinal, TcFinal FMAnusvara))
    , ("~M", (TkFinal, TcFinal FMAnusvara))
    , ("X", (TkFinal, TcFinal FMCandrabinduOrNasal))
    , ("~X", (TkFinal, TcFinal FMCandrabinduOrNasal))
    , ("H", (TkFinal, TcFinal FMVisarga))
    , ("?", (TkFinal, TcFinal FMHalanta))
    , ("^", (TkFinal, TcFinal FMCaret))
    , ("&", (TkFinal, TcFinal FMYigMgo))
    ]

wylieNumbers :: [(Text, (TokenKind, TokenCanonical))]
wylieNumbers =
    [ ("0", (TkNumber, TcNumber N0))
    , ("1", (TkNumber, TcNumber N1))
    , ("2", (TkNumber, TcNumber N2))
    , ("3", (TkNumber, TcNumber N3))
    , ("4", (TkNumber, TcNumber N4))
    , ("5", (TkNumber, TcNumber N5))
    , ("6", (TkNumber, TcNumber N6))
    , ("7", (TkNumber, TcNumber N7))
    , ("8", (TkNumber, TcNumber N8))
    , ("9", (TkNumber, TcNumber N9))
    ]

wyliePunctuation :: [(Text, (TokenKind, TokenCanonical))]
wyliePunctuation =
    [ (" ", (TkPunctuation, TcPunctuation PMTsheg))
    , ("*", (TkPunctuation, TcPunctuation PMNonBreakingTsheg))
    , ("/", (TkPunctuation, TcPunctuation PMShad))
    , ("//", (TkPunctuation, TcPunctuation PMNyisShad))
    , (";", (TkPunctuation, TcPunctuation PMTshegShad))
    , ("|", (TkPunctuation, TcPunctuation PMRinChenSpungsShad))
    , (":", (TkPunctuation, TcPunctuation PMGterTshigMgo))
    ]

wylieSymbols :: [(Text, (TokenKind, TokenCanonical))]
wylieSymbols =
    [ ("!", (TkSymbol, TcSymbol SMExclamation))
    , ("@", (TkSymbol, TcSymbol SMAt))
    , ("#", (TkSymbol, TcSymbol SMHash))
    , ("$", (TkSymbol, TcSymbol SMDollar))
    , ("%", (TkSymbol, TcSymbol SMPercent))
    , ("=", (TkSymbol, TcSymbol SMEqual))
    , ("<", (TkSymbol, TcSymbol SMLt))
    , (">", (TkSymbol, TcSymbol SMGt))
    , ("(", (TkSymbol, TcSymbol SMLParen))
    , (")", (TkSymbol, TcSymbol SMRParen))
    ]

unicodeNumbers :: [(Char, (TokenKind, TokenCanonical))]
unicodeNumbers =
    [ ('\x0f20', (TkNumber, TcNumber N0))
    , ('\x0f21', (TkNumber, TcNumber N1))
    , ('\x0f22', (TkNumber, TcNumber N2))
    , ('\x0f23', (TkNumber, TcNumber N3))
    , ('\x0f24', (TkNumber, TcNumber N4))
    , ('\x0f25', (TkNumber, TcNumber N5))
    , ('\x0f26', (TkNumber, TcNumber N6))
    , ('\x0f27', (TkNumber, TcNumber N7))
    , ('\x0f28', (TkNumber, TcNumber N8))
    , ('\x0f29', (TkNumber, TcNumber N9))
    ]

unicodeConsonants :: [(Char, (TokenKind, TokenCanonical))]
unicodeConsonants =
    [ ('\x0f40', (TkConsonant, TcConsonant Ck))
    , ('\x0f41', (TkConsonant, TcConsonant Ckh))
    , ('\x0f42', (TkConsonant, TcConsonant Cg))
    , ('\x0f43', (TkConsonant, TcConsonant CgPLUSh))
    , ('\x0f44', (TkConsonant, TcConsonant Cng))
    , ('\x0f45', (TkConsonant, TcConsonant Cc))
    , ('\x0f46', (TkConsonant, TcConsonant Cch))
    , ('\x0f47', (TkConsonant, TcConsonant Cj))
    , ('\x0f49', (TkConsonant, TcConsonant Cny))
    , ('\x0f4a', (TkConsonant, TcConsonant CT))
    , ('\x0f4b', (TkConsonant, TcConsonant CTh))
    , ('\x0f4c', (TkConsonant, TcConsonant CD))
    , ('\x0f4d', (TkConsonant, TcConsonant CDPLUSh))
    , ('\x0f4e', (TkConsonant, TcConsonant CN))
    , ('\x0f4f', (TkConsonant, TcConsonant Ct))
    , ('\x0f50', (TkConsonant, TcConsonant Cth))
    , ('\x0f51', (TkConsonant, TcConsonant Cd))
    , ('\x0f52', (TkConsonant, TcConsonant CdPLUSh))
    , ('\x0f53', (TkConsonant, TcConsonant Cn))
    , ('\x0f54', (TkConsonant, TcConsonant Cp))
    , ('\x0f55', (TkConsonant, TcConsonant Cph))
    , ('\x0f56', (TkConsonant, TcConsonant Cb))
    , ('\x0f57', (TkConsonant, TcConsonant CbPLUSh))
    , ('\x0f58', (TkConsonant, TcConsonant Cm))
    , ('\x0f59', (TkConsonant, TcConsonant Cts))
    , ('\x0f5a', (TkConsonant, TcConsonant Ctsh))
    , ('\x0f5b', (TkConsonant, TcConsonant Cdz))
    , ('\x0f5c', (TkConsonant, TcConsonant CdzPLUSh))
    , ('\x0f5d', (TkConsonant, TcConsonant Cw))
    , ('\x0f5e', (TkConsonant, TcConsonant Czh))
    , ('\x0f5f', (TkConsonant, TcConsonant Cz))
    , ('\x0f60', (TkConsonant, TcConsonant C'))
    , ('\x0f61', (TkConsonant, TcConsonant Cy))
    , ('\x0f62', (TkConsonant, TcConsonant Cr))
    , ('\x0f63', (TkConsonant, TcConsonant Cl))
    , ('\x0f64', (TkConsonant, TcConsonant Csh))
    , ('\x0f65', (TkConsonant, TcConsonant CSh))
    , ('\x0f66', (TkConsonant, TcConsonant Cs))
    , ('\x0f67', (TkConsonant, TcConsonant Ch))
    , ('\x0f68', (TkConsonant, TcConsonant Ca))
    , ('\x0f69', (TkConsonant, TcConsonant CkPLUSSh))
    , ('\x0f6a', (TkConsonant, TcConsonant CR))
    ]

unicodeSubConsonants :: [(Char, (TokenKind, TokenCanonical))]
unicodeSubConsonants =
    [ ('\x0f90', (TkSubConsonant, TcSubConsonant SCk))
    , ('\x0f91', (TkSubConsonant, TcSubConsonant SCkh))
    , ('\x0f92', (TkSubConsonant, TcSubConsonant SCg))
    , ('\x0f93', (TkSubConsonant, TcSubConsonant SCgPLUSh))
    , ('\x0f94', (TkSubConsonant, TcSubConsonant SCng))
    , ('\x0f95', (TkSubConsonant, TcSubConsonant SCc))
    , ('\x0f96', (TkSubConsonant, TcSubConsonant SCch))
    , ('\x0f97', (TkSubConsonant, TcSubConsonant SCj))
    , ('\x0f99', (TkSubConsonant, TcSubConsonant SCny))
    , ('\x0f9a', (TkSubConsonant, TcSubConsonant SCT))
    , ('\x0f9b', (TkSubConsonant, TcSubConsonant SCTh))
    , ('\x0f9c', (TkSubConsonant, TcSubConsonant SCD))
    , ('\x0f9d', (TkSubConsonant, TcSubConsonant SCDPLUSh))
    , ('\x0f9e', (TkSubConsonant, TcSubConsonant SCN))
    , ('\x0f9f', (TkSubConsonant, TcSubConsonant SCt))
    , ('\x0fa0', (TkSubConsonant, TcSubConsonant SCth))
    , ('\x0fa1', (TkSubConsonant, TcSubConsonant SCd))
    , ('\x0fa2', (TkSubConsonant, TcSubConsonant SCdPLUSh))
    , ('\x0fa3', (TkSubConsonant, TcSubConsonant SCn))
    , ('\x0fa4', (TkSubConsonant, TcSubConsonant SCp))
    , ('\x0fa5', (TkSubConsonant, TcSubConsonant SCph))
    , ('\x0fa6', (TkSubConsonant, TcSubConsonant SCb))
    , ('\x0fa7', (TkSubConsonant, TcSubConsonant SCbPLUSh))
    , ('\x0fa8', (TkSubConsonant, TcSubConsonant SCm))
    , ('\x0fa9', (TkSubConsonant, TcSubConsonant SCts))
    , ('\x0faa', (TkSubConsonant, TcSubConsonant SCtsh))
    , ('\x0fab', (TkSubConsonant, TcSubConsonant SCdz))
    , ('\x0fac', (TkSubConsonant, TcSubConsonant SCdzPLUSh))
    , ('\x0fad', (TkSubConsonant, TcSubConsonant SCw))
    , ('\x0fae', (TkSubConsonant, TcSubConsonant SCzh))
    , ('\x0faf', (TkSubConsonant, TcSubConsonant SCz))
    , ('\x0fb0', (TkSubConsonant, TcSubConsonant SC'))
    , ('\x0fb1', (TkSubConsonant, TcSubConsonant SCy))
    , ('\x0fb2', (TkSubConsonant, TcSubConsonant SCr))
    , ('\x0fb3', (TkSubConsonant, TcSubConsonant SCl))
    , ('\x0fb4', (TkSubConsonant, TcSubConsonant SCsh))
    , ('\x0fb5', (TkSubConsonant, TcSubConsonant SCSh))
    , ('\x0fb6', (TkSubConsonant, TcSubConsonant SCs))
    , ('\x0fb7', (TkSubConsonant, TcSubConsonant SCh))
    , ('\x0fb8', (TkSubConsonant, TcSubConsonant SCa))
    , ('\x0fb9', (TkSubConsonant, TcSubConsonant SCkPLUSSh))
    , ('\x0fba', (TkSubConsonant, TcSubConsonant SCW))
    , ('\x0fbb', (TkSubConsonant, TcSubConsonant SCY))
    , ('\x0fbc', (TkSubConsonant, TcSubConsonant SCR))
    ]

unicodeVowels :: [(Char, (TokenKind, TokenCanonical))]
unicodeVowels =
    [ ('\x0f71', (TkVowel, TcVowel VA))
    , ('\x0f72', (TkVowel, TcVowel Vi))
    , ('\x0f73', (TkVowel, TcVowel VI))
    , ('\x0f74', (TkVowel, TcVowel Vu))
    , ('\x0f75', (TkVowel, TcVowel VU))
    , ('\x0f7a', (TkVowel, TcVowel Ve))
    , ('\x0f7b', (TkVowel, TcVowel Vai))
    , ('\x0f7c', (TkVowel, TcVowel Vo))
    , ('\x0f7d', (TkVowel, TcVowel Vau))
    , ('\x0f80', (TkVowel, TcVowel V_i))
    ]

unicodeFinals :: [(Char, (TokenKind, TokenCanonical))]
unicodeFinals =
    [ ('\x0f7e', (TkFinal, TcFinal FMAnusvara))
    , ('\x0f82', (TkFinal, TcFinal FMAnusvara))
    , ('\x0f83', (TkFinal, TcFinal FMAnusvara))
    , ('\x0f37', (TkFinal, TcFinal FMCandrabinduOrNasal))
    , ('\x0f35', (TkFinal, TcFinal FMCandrabinduOrNasal))
    , ('\x0f39', (TkFinal, TcFinal FMCaret))
    , ('\x0f7f', (TkFinal, TcFinal FMVisarga))
    , ('\x0f84', (TkFinal, TcFinal FMHalanta))
    , ('\x0f85', (TkFinal, TcFinal FMYigMgo))
    ]

unicodePunctuation :: [(Char, (TokenKind, TokenCanonical))]
unicodePunctuation =
    [ ('\x0f0b', (TkPunctuation, TcPunctuation PMTsheg))
    , ('\x0f0c', (TkPunctuation, TcPunctuation PMNonBreakingTsheg))
    , ('\x0f0d', (TkPunctuation, TcPunctuation PMShad))
    , ('\x0f0e', (TkPunctuation, TcPunctuation PMNyisShad))
    , ('\x0f0f', (TkPunctuation, TcPunctuation PMTshegShad))
    , ('\x0f11', (TkPunctuation, TcPunctuation PMRinChenSpungsShad))
    , ('\x0f14', (TkPunctuation, TcPunctuation PMGterTshigMgo))
    ]

unicodeSymbols :: [(Char, (TokenKind, TokenCanonical))]
unicodeSymbols =
    [ ('\x0f08', (TkSymbol, TcSymbol SMExclamation))
    , ('\x0f04', (TkSymbol, TcSymbol SMAt))
    , ('\x0f05', (TkSymbol, TcSymbol SMHash))
    , ('\x0f06', (TkSymbol, TcSymbol SMDollar))
    , ('\x0f07', (TkSymbol, TcSymbol SMPercent))
    , ('\x0f34', (TkSymbol, TcSymbol SMEqual))
    , ('\x0f3a', (TkSymbol, TcSymbol SMLt))
    , ('\x0f3b', (TkSymbol, TcSymbol SMGt))
    , ('\x0f3c', (TkSymbol, TcSymbol SMLParen))
    , ('\x0f3d', (TkSymbol, TcSymbol SMRParen))
    ]

aliasesWylie :: String -> [Text] -> (TokenKind, TokenCanonical) -> (String, Assertion)
aliasesWylie name raws expected =
    (name, mapM_ (`singleWylie` expected) raws)

aliasesUnicode :: String -> [Char] -> (TokenKind, TokenCanonical) -> (String, Assertion)
aliasesUnicode name raws expected =
    (name, mapM_ (\ch -> singleUnicode (T.singleton ch) expected) raws)

assertWylieSingles :: [(Text, (TokenKind, TokenCanonical))] -> Assertion
assertWylieSingles = mapM_ (\(raw, expected) -> singleWylie raw expected)

assertUnicodeSingles :: [(Char, (TokenKind, TokenCanonical))] -> Assertion
assertUnicodeSingles = mapM_ (\(ch, expected) -> singleUnicode (T.singleton ch) expected)
