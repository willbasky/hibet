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
        , testGroup "wylie expansions" (map mkCase wylieExpansionCases)
        , testGroup "final classes" (map mkCase finalClassCases)
        , testGroup "wylie tables" (map mkCase wylieTableCases)
        , testGroup "unicode" (map mkCase unicodeCases)
        , testGroup "unicode decomposition" (map mkCase unicodeDecompositionCases)
        , testGroup "unicode tables" (map mkCase unicodeTableCases)
        ]

mkCase :: (String, Assertion) -> TestTree
mkCase (name, assertion) = testCase name assertion

singleWylie :: Text -> (TokenKind, TokenCanonical) -> Assertion
singleWylie input expected =
    case fst (tokenizeWylie input) of
        [tok] -> (tokenKind tok, tokenCanonical tok) @?= expected
        xs -> error $ "Expected 1 token, got " <> show (length xs) <> " for input: " <> show input

singleUnicode :: Text -> (TokenKind, TokenCanonical) -> Assertion
singleUnicode input expected =
    case fst (tokenizeUnicode input) of
        [tok] -> (tokenKind tok, tokenCanonical tok) @?= expected
        xs -> error $ "Expected 1 token, got " <> show (length xs) <> " for input: " <> show input

-- | Expect the whole input to tokenize to exactly the given canonical
-- sequence (a single spelling may expand or decompose into several tokens).
seqWylie :: Text -> [TokenCanonical] -> Assertion
seqWylie input expected =
    map tokenCanonical (fst (tokenizeWylie input)) @?= expected

seqUnicode :: Text -> [TokenCanonical] -> Assertion
seqUnicode input expected =
    map tokenCanonical (fst (tokenizeUnicode input)) @?= expected

wylieCases :: [(String, Assertion)]
wylieCases =
    [ ("consonant basic k", singleWylie "k" (TkConsonant, TcConsonant Ck))
    , ("consonant aspirated kh", singleWylie "kh" (TkConsonant, TcConsonant Ckh))
    , ("consonant alias W -> Cw", singleWylie "W" (TkConsonant, TcConsonant Cw))
    , ("vowel short i", singleWylie "i" (TkVowel, TcVowel Vi))
    , ("vowel composite au", singleWylie "au" (TkVowel, TcVowel Vau))
    , ("final anusvara M", singleWylie "M" (TkFinal, TcFinal FMAnusvara))
    , ("final bindu nAda ~M`", singleWylie "~M`" (TkFinal, TcFinal FMBinduNada))
    , ("final candrabindu ~M", singleWylie "~M" (TkFinal, TcFinal FMCandrabindu))
    , ("final srog med X", singleWylie "X" (TkFinal, TcFinal FMSrogMed))
    , ("final nasal variant ~X", singleWylie "~X" (TkFinal, TcFinal FMCandrabinduHalanta))
    , ("final visarga H", singleWylie "H" (TkFinal, TcFinal FMVisarga))
    , ("number 0", singleWylie "0" (TkNumber, TcNumber N0))
    , ("number 9", singleWylie "9" (TkNumber, TcNumber N9))
    , ("punctuation tsheg space", singleWylie " " (TkPunctuation, TcPunctuation PMTsheg))
    , ("punctuation double shad", singleWylie "//" (TkPunctuation, TcPunctuation PMNyisShad))
    , ("punctuation gter tshig mgo", singleWylie ":" (TkPunctuation, TcPunctuation PMGterTshigMgo))
    , ("symbol exclamation", singleWylie "!" (TkSymbol, TcSymbol SMExclamation))
    , ("symbol right paren", singleWylie ")" (TkSymbol, TcSymbol SMRParen))
    , ("explicit space marker underscore", singleWylie "_" (TkSpace, TcSpace SMSpace))
    , ("explicit plus becomes ConSpec", singleWylie "+" (TkConSpec, TcConSpec CSPlus))
    , ("explicit dot becomes ConSpec", singleWylie "." (TkConSpec, TcConSpec CSDot))
    , ("unknown latin x becomes unknown", singleWylie "x" (TkUnknown, TcUnknown (UnknownMark "x")))
    , ("escape decodes consonant kka", singleWylie "\\u0F6B" (TkConsonant, TcConsonant Ckka))
    , ("escape lower-case hex decodes too", singleWylie "\\u0f6c" (TkConsonant, TcConsonant CRra))
    , ("escape decodes vowel vocalic r-i as sequence", seqWylie "\\u0F76" [TcSubConsonant SCr, TcVowel V_i])
    , ("escape decodes punctuation nyis tsheg shad", singleWylie "\\u0F10" (TkPunctuation, TcPunctuation PMNyisTshegShad))
    , ("escape decodes half number H_1", singleWylie "\\u0F2A" (TkHalfNumber, TcHalfNumber H_1))
    , ("escape decodes sign yig mgo at", singleWylie "\\u0F00" (TkSign, TcSign SGYigMgoAt))
    , ("escape decodes sanskrit mark", singleWylie "\\u0F86" (TkSanskritMark, TcSanskritMark SMiLciRtags))
    , ("escape decodes ornament", singleWylie "\\u0FD0" (TkOrnament, TcOrnament OMRdelDkarGcig))
    , ("escape to unknown codepoint stays unknown", singleWylie "\\u0E0E" (TkUnknown, TcUnknown (UnknownMark "\\u0E0E")))
    , ("U escape out of Tibetan stays unknown", singleWylie "\\U0001F600" (TkUnknown, TcUnknown (UnknownMark "\\U0001F600")))
    ]

wylieAliasCases :: [(String, Assertion)]
wylieAliasCases =
    [ aliasesWylie "retroflex ta aliases" ["T", "-t"] (TkConsonant, TcConsonant CT)
    , aliasesWylie "retroflex tha aliases" ["Th", "-th"] (TkConsonant, TcConsonant CTh)
    , aliasesWylie "retroflex da aliases" ["D", "-d"] (TkConsonant, TcConsonant CD)
    , aliasesWylie "retroflex na aliases" ["N", "-n"] (TkConsonant, TcConsonant CN)
    , aliasesWylie "sha aliases" ["Sh", "-sh"] (TkConsonant, TcConsonant CSh)
    , aliasesWylie "wa aliases" ["w", "W"] (TkConsonant, TcConsonant Cw)
    , aliasesWylie "ya aliases" ["y", "Y"] (TkConsonant, TcConsonant Cy)
    ]

-- | Compound spellings that expand to several canonical tokens: aspirates
-- (consonant + subjoined-h), long vowels (A + vowel) and vocalic r/l
-- (subjoined r/l + reverse-i).
wylieExpansionCases :: [(String, Assertion)]
wylieExpansionCases =
    [ seqAliasesWylie "gh expansions" ["gh", "g+h"] [TcConsonant Cg, TcSubConsonant SCh]
    , seqAliasesWylie "Dh expansions" ["Dh", "D+h"] [TcConsonant CD, TcSubConsonant SCh]
    , seqAliasesWylie "dh expansions" ["dh", "d+h"] [TcConsonant Cd, TcSubConsonant SCh]
    , seqAliasesWylie "bh expansions" ["bh", "b+h"] [TcConsonant Cb, TcSubConsonant SCh]
    , seqAliasesWylie "dzh expansions" ["dzh", "dz+h"] [TcConsonant Cdz, TcSubConsonant SCh]
    , ("k+Sh expansion", seqWylie "k+Sh" [TcConsonant Ck, TcSubConsonant SCSh])
    , ("f caret expansion", seqWylie "f" [TcConsonant Cph, TcFinal FMCaret])
    , ("v caret expansion", seqWylie "v" [TcConsonant Cb, TcFinal FMCaret])
    , seqAliasesWylie "subjoined Dh expansions" ["-dh", "-d+h"] [TcSubConsonant SCD, TcSubConsonant SCh]
    , ("long I", seqWylie "I" [TcVowel VA, TcVowel Vi])
    , ("long U", seqWylie "U" [TcVowel VA, TcVowel Vu])
    , ("long E", seqWylie "E" [TcVowel VA, TcVowel Ve])
    , ("long O", seqWylie "O" [TcVowel VA, TcVowel Vo])
    , ("long -I", seqWylie "-I" [TcVowel VA, TcVowel V_i])
    , ("vocalic r-i", seqWylie "r-i" [TcSubConsonant SCr, TcVowel V_i])
    , ("vocalic r-I", seqWylie "r-I" [TcSubConsonant SCr, TcVowel VA, TcVowel V_i])
    , ("vocalic l-i", seqWylie "l-i" [TcSubConsonant SCl, TcVowel V_i])
    , ("vocalic l-I", seqWylie "l-I" [TcSubConsonant SCl, TcVowel VA, TcVowel V_i])
    ]

wylieTableCases :: [(String, Assertion)]
wylieTableCases =
    [ ("wylie consonants full table", assertWylieSingles wylieConsonants)
    , ("wylie vowels full table", assertWylieSingles wylieVowels)
    , ("wylie finals full table", assertWylieSingles wylieFinals)
    , ("wylie numbers full table", assertWylieSingles wylieNumbers)
    , ("wylie half-numbers full table", assertWylieSingles wylieHalfNumbers)
    , ("wylie punctuation full table", assertWylieSingles wyliePunctuation)
    , ("wylie signs full table", assertWylieSingles wylieSigns)
    , ("wylie sanskrit marks full table", assertWylieSingles wylieSanskritMarks)
    , ("wylie ornaments full table", assertWylieSingles wylieOrnaments)
    , ("wylie symbols full table", assertWylieSingles wylieSymbols)
    ]

unicodeCases :: [(String, Assertion)]
unicodeCases =
    [ ("consonant basic ka", singleUnicode "ཀ" (TkConsonant, TcConsonant Ck))
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
    , ("sign yig mgo at", singleUnicode "\x0f00" (TkSign, TcSign SGYigMgoAt))
    , ("half number H_4", singleUnicode "\x0f2d" (TkHalfNumber, TcHalfNumber H_4))
    , ("sanskrit mark i lci rtags", singleUnicode "྆" (TkSanskritMark, TcSanskritMark SMiLciRtags))
    , ("ornament rdel dkar gcig", singleUnicode "\x0fd0" (TkOrnament, TcOrnament OMRdelDkarGcig))
    , ("punctuation nyis tsheg shad", singleUnicode "\x0f10" (TkPunctuation, TcPunctuation PMNyisTshegShad))
    , ("ascii space becomes TkSpace", singleUnicode " " (TkSpace, TcSpace SMSpace))
    , ("unknown latin x becomes unknown", singleUnicode "x" (TkUnknown, TcUnknown (UnknownMark "x")))
    ]

-- | Deprecated precomposed characters decompose into canonical token
-- sequences on input (aspirates, long vowels, vocalic r/l).
unicodeDecompositionCases :: [(String, Assertion)]
unicodeDecompositionCases =
    [ ("precomposed U+0F43 (གྷ) decomposes", seqUnicode "\x0f43" [TcConsonant Cg, TcSubConsonant SCh])
    , ("precomposed U+0F4D (ཌྷ) decomposes", seqUnicode "\x0f4d" [TcConsonant CD, TcSubConsonant SCh])
    , ("precomposed U+0F52 (དྷ) decomposes", seqUnicode "\x0f52" [TcConsonant Cd, TcSubConsonant SCh])
    , ("precomposed U+0F57 (བྷ) decomposes", seqUnicode "\x0f57" [TcConsonant Cb, TcSubConsonant SCh])
    , ("precomposed U+0F5C (ཛྷ) decomposes", seqUnicode "\x0f5c" [TcConsonant Cdz, TcSubConsonant SCh])
    , ("precomposed U+0F69 (ཀྵ) decomposes", seqUnicode "\x0f69" [TcConsonant Ck, TcSubConsonant SCSh])
    , ("precomposed U+0FB9 (ྐྵ) decomposes", seqUnicode "\x0fb9" [TcSubConsonant SCk, TcSubConsonant SCSh])
    , ("precomposed U+0F93 (ྒྷ) decomposes", seqUnicode "\x0f93" [TcSubConsonant SCg, TcSubConsonant SCh])
    , ("precomposed U+0F9D (ྜྷ) decomposes", seqUnicode "\x0f9d" [TcSubConsonant SCD, TcSubConsonant SCh])
    , ("precomposed U+0FA2 (ྡྷ) decomposes", seqUnicode "\x0fa2" [TcSubConsonant SCd, TcSubConsonant SCh])
    , ("precomposed U+0FA7 (ྦྷ) decomposes", seqUnicode "\x0fa7" [TcSubConsonant SCb, TcSubConsonant SCh])
    , ("precomposed U+0FAC (ྫྷ) decomposes", seqUnicode "\x0fac" [TcSubConsonant SCdz, TcSubConsonant SCh])
    , ("precomposed U+0F73 (ཱི) decomposes", seqUnicode "\x0f73" [TcVowel VA, TcVowel Vi])
    , ("precomposed U+0F75 (ཱུ) decomposes", seqUnicode "\x0f75" [TcVowel VA, TcVowel Vu])
    , ("precomposed U+0F81 (ཱྀ) decomposes", seqUnicode "\x0f81" [TcVowel VA, TcVowel V_i])
    , ("precomposed U+0F76 (ྲྀ) decomposes", seqUnicode "\x0f76" [TcSubConsonant SCr, TcVowel V_i])
    , ("precomposed U+0F77 (ཷ) decomposes", seqUnicode "\x0f77" [TcSubConsonant SCr, TcVowel VA, TcVowel V_i])
    , ("precomposed U+0F78 (ླྀ) decomposes", seqUnicode "\x0f78" [TcSubConsonant SCl, TcVowel V_i])
    , ("precomposed U+0F79 (ཹ) decomposes", seqUnicode "\x0f79" [TcSubConsonant SCl, TcVowel VA, TcVowel V_i])
    ]

unicodeTableCases :: [(String, Assertion)]
unicodeTableCases =
    [ ("unicode consonants full table", assertUnicodeSingles unicodeConsonants)
    , ("unicode sub-consonants full table", assertUnicodeSingles unicodeSubConsonants)
    , ("unicode vowels full table", assertUnicodeSingles unicodeVowels)
    , ("unicode finals full table", assertUnicodeSingles unicodeFinals)
    , ("unicode numbers full table", assertUnicodeSingles unicodeNumbers)
    , ("unicode half-numbers full table", assertUnicodeSingles unicodeHalfNumbers)
    , ("unicode punctuation full table", assertUnicodeSingles unicodePunctuation)
    , ("unicode signs full table", assertUnicodeSingles unicodeSigns)
    , ("unicode sanskrit marks full table", assertUnicodeSingles unicodeSanskritMarks)
    , ("unicode ornaments full table", assertUnicodeSingles unicodeOrnaments)
    , ("unicode symbols full table", assertUnicodeSingles unicodeSymbols)
    ]

wylieConsonants :: [(Text, (TokenKind, TokenCanonical))]
wylieConsonants =
    [ ("k", (TkConsonant, TcConsonant Ck))
    , ("kh", (TkConsonant, TcConsonant Ckh))
    , ("g", (TkConsonant, TcConsonant Cg))
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
    , ("N", (TkConsonant, TcConsonant CN))
    , ("-n", (TkConsonant, TcConsonant CN))
    , ("t", (TkConsonant, TcConsonant Ct))
    , ("th", (TkConsonant, TcConsonant Cth))
    , ("d", (TkConsonant, TcConsonant Cd))
    , ("n", (TkConsonant, TcConsonant Cn))
    , ("p", (TkConsonant, TcConsonant Cp))
    , ("ph", (TkConsonant, TcConsonant Cph))
    , ("b", (TkConsonant, TcConsonant Cb))
    , ("m", (TkConsonant, TcConsonant Cm))
    , ("ts", (TkConsonant, TcConsonant Cts))
    , ("tsh", (TkConsonant, TcConsonant Ctsh))
    , ("dz", (TkConsonant, TcConsonant Cdz))
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
    , ("R", (TkConsonant, TcConsonant CR))
    ]

wylieVowels :: [(Text, (TokenKind, TokenCanonical))]
wylieVowels =
    [ ("A", (TkVowel, TcVowel VA))
    , ("i", (TkVowel, TcVowel Vi))
    , ("u", (TkVowel, TcVowel Vu))
    , ("e", (TkVowel, TcVowel Ve))
    , ("ai", (TkVowel, TcVowel Vai))
    , ("o", (TkVowel, TcVowel Vo))
    , ("au", (TkVowel, TcVowel Vau))
    , ("-i", (TkVowel, TcVowel V_i))
    ]

wylieFinals :: [(Text, (TokenKind, TokenCanonical))]
wylieFinals =
    [ ("M", (TkFinal, TcFinal FMAnusvara))
    , ("~M`", (TkFinal, TcFinal FMBinduNada))
    , ("~M", (TkFinal, TcFinal FMCandrabindu))
    , ("X", (TkFinal, TcFinal FMSrogMed))
    , ("~X", (TkFinal, TcFinal FMCandrabinduHalanta))
    , ("H", (TkFinal, TcFinal FMVisarga))
    , ("?", (TkFinal, TcFinal FMHalanta))
    , ("^", (TkFinal, TcFinal FMCaret))
    , ("&", (TkFinal, TcFinal FMYigMgo))
    ]

-- | Orthographic classes of the nine finals (at most one member per class in
-- a syllable, used by the later spelling check).
finalClassCases :: [(String, Assertion)]
finalClassCases =
    [ ("M class for anusvara", finalClass FMAnusvara @?= "M")
    , ("M class for bindu nAda", finalClass FMBinduNada @?= "M")
    , ("M class for candrabindu", finalClass FMCandrabindu @?= "M")
    , ("X class for srog med", finalClass FMSrogMed @?= "X")
    , ("X class for candrabindu halanta", finalClass FMCandrabinduHalanta @?= "X")
    , ("H class for visarga", finalClass FMVisarga @?= "H")
    , ("? class for halanta", finalClass FMHalanta @?= "?")
    , ("^ class for caret", finalClass FMCaret @?= "^")
    , ("& class for yig mgo", finalClass FMYigMgo @?= "&")
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

wylieHalfNumbers :: [(Text, (TokenKind, TokenCanonical))]
wylieHalfNumbers =
    [ ("\\u0F2A", (TkHalfNumber, TcHalfNumber H_1))
    , ("\\u0F2B", (TkHalfNumber, TcHalfNumber H_2))
    , ("\\u0F2C", (TkHalfNumber, TcHalfNumber H_3))
    , ("\\u0F2D", (TkHalfNumber, TcHalfNumber H_4))
    , ("\\u0F2E", (TkHalfNumber, TcHalfNumber H_5))
    , ("\\u0F2F", (TkHalfNumber, TcHalfNumber H_6))
    , ("\\u0F30", (TkHalfNumber, TcHalfNumber H_7))
    , ("\\u0F31", (TkHalfNumber, TcHalfNumber H_8))
    , ("\\u0F32", (TkHalfNumber, TcHalfNumber H_9))
    , ("\\u0F33", (TkHalfNumber, TcHalfNumber H_0))
    ]

wylieSigns :: [(Text, (TokenKind, TokenCanonical))]
wylieSigns =
    [ ("\\u0F00", (TkSign, TcSign SGYigMgoAt))
    , ("\\u0F01", (TkSign, TcSign SGKaKhaGaGsum))
    , ("\\u0F02", (TkSign, TcSign SGNyiZlaNaaDa))
    , ("\\u0F03", (TkSign, TcSign SGSbrulShad))
    ]

wylieSanskritMarks :: [(Text, (TokenKind, TokenCanonical))]
wylieSanskritMarks =
    [ ("\\u0F86", (TkSanskritMark, TcSanskritMark SMiLciRtags))
    , ("\\u0F87", (TkSanskritMark, TcSanskritMark SMiYangRtags))
    , ("\\u0F8D", (TkSanskritMark, TcSanskritMark SMiLceTsaCanSubjoined))
    , ("\\u0F8E", (TkSanskritMark, TcSanskritMark SMiMchuCanSubjoined))
    , ("\\u0F8F", (TkSanskritMark, TcSanskritMark SMiInvertedMchuCanSubjoined))
    ]

wylieOrnaments :: [(Text, (TokenKind, TokenCanonical))]
wylieOrnaments =
    [ ("\\u0FD0", (TkOrnament, TcOrnament OMRdelDkarGcig))
    , ("\\u0FD1", (TkOrnament, TcOrnament OMRdelDkarGnyis))
    , ("\\u0FD2", (TkOrnament, TcOrnament OMRdelDkarGsum))
    , ("\\u0FD3", (TkOrnament, TcOrnament OMRdelNagGcig))
    , ("\\u0FD4", (TkOrnament, TcOrnament OMRdelNagGnyis))
    , ("\\u0FD9", (TkOrnament, TcOrnament OMLeadingMchanRtags))
    , ("\\u0FDA", (TkOrnament, TcOrnament OMTrailingMchanRtags))
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

unicodeHalfNumbers :: [(Char, (TokenKind, TokenCanonical))]
unicodeHalfNumbers =
    [ ('\x0f2a', (TkHalfNumber, TcHalfNumber H_1))
    , ('\x0f2b', (TkHalfNumber, TcHalfNumber H_2))
    , ('\x0f2c', (TkHalfNumber, TcHalfNumber H_3))
    , ('\x0f2d', (TkHalfNumber, TcHalfNumber H_4))
    , ('\x0f2e', (TkHalfNumber, TcHalfNumber H_5))
    , ('\x0f2f', (TkHalfNumber, TcHalfNumber H_6))
    , ('\x0f30', (TkHalfNumber, TcHalfNumber H_7))
    , ('\x0f31', (TkHalfNumber, TcHalfNumber H_8))
    , ('\x0f32', (TkHalfNumber, TcHalfNumber H_9))
    , ('\x0f33', (TkHalfNumber, TcHalfNumber H_0))
    ]

unicodeSigns :: [(Char, (TokenKind, TokenCanonical))]
unicodeSigns =
    [ ('\x0f00', (TkSign, TcSign SGYigMgoAt))
    , ('\x0f01', (TkSign, TcSign SGKaKhaGaGsum))
    , ('\x0f02', (TkSign, TcSign SGNyiZlaNaaDa))
    , ('\x0f03', (TkSign, TcSign SGSbrulShad))
    ]

unicodeSanskritMarks :: [(Char, (TokenKind, TokenCanonical))]
unicodeSanskritMarks =
    [ ('\x0f86', (TkSanskritMark, TcSanskritMark SMiLciRtags))
    , ('\x0f87', (TkSanskritMark, TcSanskritMark SMiYangRtags))
    , ('\x0f8d', (TkSanskritMark, TcSanskritMark SMiLceTsaCanSubjoined))
    , ('\x0f8e', (TkSanskritMark, TcSanskritMark SMiMchuCanSubjoined))
    , ('\x0f8f', (TkSanskritMark, TcSanskritMark SMiInvertedMchuCanSubjoined))
    ]

unicodeOrnaments :: [(Char, (TokenKind, TokenCanonical))]
unicodeOrnaments =
    [ ('\x0fd0', (TkOrnament, TcOrnament OMRdelDkarGcig))
    , ('\x0fd1', (TkOrnament, TcOrnament OMRdelDkarGnyis))
    , ('\x0fd2', (TkOrnament, TcOrnament OMRdelDkarGsum))
    , ('\x0fd3', (TkOrnament, TcOrnament OMRdelNagGcig))
    , ('\x0fd4', (TkOrnament, TcOrnament OMRdelNagGnyis))
    , ('\x0fd9', (TkOrnament, TcOrnament OMLeadingMchanRtags))
    , ('\x0fda', (TkOrnament, TcOrnament OMTrailingMchanRtags))
    ]

unicodeConsonants :: [(Char, (TokenKind, TokenCanonical))]
unicodeConsonants =
    [ ('\x0f40', (TkConsonant, TcConsonant Ck))
    , ('\x0f41', (TkConsonant, TcConsonant Ckh))
    , ('\x0f42', (TkConsonant, TcConsonant Cg))
    , ('\x0f44', (TkConsonant, TcConsonant Cng))
    , ('\x0f45', (TkConsonant, TcConsonant Cc))
    , ('\x0f46', (TkConsonant, TcConsonant Cch))
    , ('\x0f47', (TkConsonant, TcConsonant Cj))
    , ('\x0f49', (TkConsonant, TcConsonant Cny))
    , ('\x0f4a', (TkConsonant, TcConsonant CT))
    , ('\x0f4b', (TkConsonant, TcConsonant CTh))
    , ('\x0f4c', (TkConsonant, TcConsonant CD))
    , ('\x0f4e', (TkConsonant, TcConsonant CN))
    , ('\x0f4f', (TkConsonant, TcConsonant Ct))
    , ('\x0f50', (TkConsonant, TcConsonant Cth))
    , ('\x0f51', (TkConsonant, TcConsonant Cd))
    , ('\x0f53', (TkConsonant, TcConsonant Cn))
    , ('\x0f54', (TkConsonant, TcConsonant Cp))
    , ('\x0f55', (TkConsonant, TcConsonant Cph))
    , ('\x0f56', (TkConsonant, TcConsonant Cb))
    , ('\x0f58', (TkConsonant, TcConsonant Cm))
    , ('\x0f59', (TkConsonant, TcConsonant Cts))
    , ('\x0f5a', (TkConsonant, TcConsonant Ctsh))
    , ('\x0f5b', (TkConsonant, TcConsonant Cdz))
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
    , ('\x0f6a', (TkConsonant, TcConsonant CR))
    , ('\x0f6b', (TkConsonant, TcConsonant Ckka))
    , ('\x0f6c', (TkConsonant, TcConsonant CRra))
    ]

unicodeSubConsonants :: [(Char, (TokenKind, TokenCanonical))]
unicodeSubConsonants =
    [ ('\x0f90', (TkSubConsonant, TcSubConsonant SCk))
    , ('\x0f91', (TkSubConsonant, TcSubConsonant SCkh))
    , ('\x0f92', (TkSubConsonant, TcSubConsonant SCg))
    , ('\x0f94', (TkSubConsonant, TcSubConsonant SCng))
    , ('\x0f95', (TkSubConsonant, TcSubConsonant SCc))
    , ('\x0f96', (TkSubConsonant, TcSubConsonant SCch))
    , ('\x0f97', (TkSubConsonant, TcSubConsonant SCj))
    , ('\x0f99', (TkSubConsonant, TcSubConsonant SCny))
    , ('\x0f9a', (TkSubConsonant, TcSubConsonant SCT))
    , ('\x0f9b', (TkSubConsonant, TcSubConsonant SCTh))
    , ('\x0f9c', (TkSubConsonant, TcSubConsonant SCD))
    , ('\x0f9e', (TkSubConsonant, TcSubConsonant SCN))
    , ('\x0f9f', (TkSubConsonant, TcSubConsonant SCt))
    , ('\x0fa0', (TkSubConsonant, TcSubConsonant SCth))
    , ('\x0fa1', (TkSubConsonant, TcSubConsonant SCd))
    , ('\x0fa3', (TkSubConsonant, TcSubConsonant SCn))
    , ('\x0fa4', (TkSubConsonant, TcSubConsonant SCp))
    , ('\x0fa5', (TkSubConsonant, TcSubConsonant SCph))
    , ('\x0fa6', (TkSubConsonant, TcSubConsonant SCb))
    , ('\x0fa8', (TkSubConsonant, TcSubConsonant SCm))
    , ('\x0fa9', (TkSubConsonant, TcSubConsonant SCts))
    , ('\x0faa', (TkSubConsonant, TcSubConsonant SCtsh))
    , ('\x0fab', (TkSubConsonant, TcSubConsonant SCdz))
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
    , ('\x0fba', (TkSubConsonant, TcSubConsonant SCW))
    , ('\x0fbb', (TkSubConsonant, TcSubConsonant SCY))
    , ('\x0fbc', (TkSubConsonant, TcSubConsonant SCR))
    ]

unicodeVowels :: [(Char, (TokenKind, TokenCanonical))]
unicodeVowels =
    [ ('\x0f71', (TkVowel, TcVowel VA))
    , ('\x0f72', (TkVowel, TcVowel Vi))
    , ('\x0f74', (TkVowel, TcVowel Vu))
    , ('\x0f7a', (TkVowel, TcVowel Ve))
    , ('\x0f7b', (TkVowel, TcVowel Vai))
    , ('\x0f7c', (TkVowel, TcVowel Vo))
    , ('\x0f7d', (TkVowel, TcVowel Vau))
    , ('\x0f80', (TkVowel, TcVowel V_i))
    ]

unicodeFinals :: [(Char, (TokenKind, TokenCanonical))]
unicodeFinals =
    [ ('\x0f7e', (TkFinal, TcFinal FMAnusvara))
    , ('\x0f82', (TkFinal, TcFinal FMBinduNada))
    , ('\x0f83', (TkFinal, TcFinal FMCandrabindu))
    , ('\x0f37', (TkFinal, TcFinal FMSrogMed))
    , ('\x0f35', (TkFinal, TcFinal FMCandrabinduHalanta))
    , ('\x0f7f', (TkFinal, TcFinal FMVisarga))
    , ('\x0f84', (TkFinal, TcFinal FMHalanta))
    , ('\x0f39', (TkFinal, TcFinal FMCaret))
    , ('\x0f85', (TkFinal, TcFinal FMYigMgo))
    ]

unicodePunctuation :: [(Char, (TokenKind, TokenCanonical))]
unicodePunctuation =
    [ ('\x0f0b', (TkPunctuation, TcPunctuation PMTsheg))
    , ('\x0f0c', (TkPunctuation, TcPunctuation PMNonBreakingTsheg))
    , ('\x0f0d', (TkPunctuation, TcPunctuation PMShad))
    , ('\x0f0e', (TkPunctuation, TcPunctuation PMNyisShad))
    , ('\x0f0f', (TkPunctuation, TcPunctuation PMTshegShad))
    , ('\x0f10', (TkPunctuation, TcPunctuation PMNyisTshegShad))
    , ('\x0f11', (TkPunctuation, TcPunctuation PMRinChenSpungsShad))
    , ('\x0f12', (TkPunctuation, TcPunctuation PMRgyaGramShad))
    , ('\x0f13', (TkPunctuation, TcPunctuation PMCaretDzudRtagsMeLong))
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

seqAliasesWylie :: String -> [Text] -> [TokenCanonical] -> (String, Assertion)
seqAliasesWylie name raws expected =
    (name, mapM_ (`seqWylie` expected) raws)

assertWylieSingles :: [(Text, (TokenKind, TokenCanonical))] -> Assertion
assertWylieSingles = mapM_ (\(raw, expected) -> singleWylie raw expected)

assertUnicodeSingles :: [(Char, (TokenKind, TokenCanonical))] -> Assertion
assertUnicodeSingles = mapM_ (\(ch, expected) -> singleUnicode (T.singleton ch) expected)
