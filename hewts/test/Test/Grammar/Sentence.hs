module Test.Grammar.Sentence (tests) where

import Convert.Grammar.Parser (parseEither)
import Convert.Grammar.Sentence (SpellItem (..), pSentence)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (either)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, (@?=), testCase)

tests :: TestTree
tests =
    testGroup
        "sentence dispatcher"
        [ simple
        , leadingPunct
        , spaceRun
        , realWords
        , unknownAscii
        , numbers
        , longA
        , roundtrip
        , leadingShad
        , doubleShad
        , newline
        , digtsAfterSpace
        , consecutivePunct
        , allOther
        , longTextParses
        ]

itemRaws :: SpellItem -> [Text]
itemRaws (Syllable ts) = map tokenRaw ts
itemRaws (Number ts) = map tokenRaw ts
itemRaws (Punct ts) = map tokenRaw ts
itemRaws (Other ts) = map tokenRaw ts

tagged :: [SpellItem] -> [(Text, [Text])]
tagged = map $ \i -> case i of
    Syllable ts -> ("S", map tokenRaw ts)
    Number ts -> ("N", map tokenRaw ts)
    Punct ts -> ("P", map tokenRaw ts)
    Other ts -> ("O", map tokenRaw ts)

rawsOf :: [SpellItem] -> [Text]
rawsOf = concatMap itemRaws

run :: Text -> [SpellItem]
run = either (const []) id . parseEither pSentence . tokenizeUnicode

simple :: TestTree
simple =
    testCase "དེ་དུ་ -> syllables around tsheg" $
        tagged (run "དེ་དུ་")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", ["་"])
                , ("S", ["ད", "ུ"])
                , ("P", ["་"])
                ]

leadingPunct :: TestTree
leadingPunct =
    testCase "་དེ -> leading tsheg kept as Punct" $
        tagged (run "་དེ")
            @?= [ ("P", ["་"])
                , ("S", ["ད", "ེ"])
                ]

spaceRun :: TestTree
spaceRun =
    testCase "དེ དུ -> space is punctuation-like" $
        tagged (run "དེ དུ")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", [" "])
                , ("S", ["ད", "ུ"])
                ]

realWords :: TestTree
realWords =
    testCase "མཆོག་དེ་རིང་ཕྱི་ཚེས་དུ་སུ་ -> every syllable recognized" $
        tagged (run "མཆོག་དེ་རིང་ཕྱི་ཚེས་དུ་སུ་")
            @?= [ ("S", ["མ", "ཆ", "ོ", "ག"])
                , ("P", ["་"])
                , ("S", ["ད", "ེ"])
                , ("P", ["་"])
                , ("S", ["ར", "ི", "ང"])
                , ("P", ["་"])
                , ("S", ["ཕ", "ྱ", "ི"])
                , ("P", ["་"])
                , ("S", ["ཚ", "ེ", "ས"])
                , ("P", ["་"])
                , ("S", ["ད", "ུ"])
                , ("P", ["་"])
                , ("S", ["ས", "ུ"])
                , ("P", ["་"])
                ]

unknownAscii :: TestTree
unknownAscii =
    testCase "དེ་xyz་ -> unrecognized tokens kept as Other" $
        tagged (run "དེ་xyz་")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", ["་"])
                , ("O", ["x", "y", "z"])
                , ("P", ["་"])
                ]

numbers :: TestTree
numbers =
    testCase "དེ་༡༢་ -> number tokens grouped as Number" $
        tagged (run "དེ་༡༢་")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", ["་"])
                , ("N", ["༡", "༢"])
                , ("P", ["་"])
                ]

longA :: TestTree
longA =
    testCase "ཊཱ -> long vowel of a Sanskrit root is not swallowed" $
        tagged (run "ཊཱ")
            @?= [ ("S", ["ཊ"])
                , ("O", ["ཱ"])
                ]

roundtrip :: TestTree
roundtrip =
    testCase "every token is preserved, nothing dropped" $ do
        let inputs =
                [ "མཆོག་དེ་རིང་ཕྱི་ཚེས་དུ་སུ་"
                , "།མི་ཡིན་ དེ།"
                , "དེ་xyz་༡༢ ཊཱ"
                ]
        mapM_ check inputs
  where
    check input =
        rawsOf (run input) @?= map tokenRaw (tokenizeUnicode input)

longText :: Text
longText =
    "འོན་ཀྱང་དེ་རིང་ཉེ་སྔོན་གྱི་ལག་རྩལ་པས་ཡིག་ཟམ་ཞིག་བསྐུར་བར་ཀུ་ཤུས་རིན་མེད་ཞུབས་ཞུ་བྱེད་མཚམས་བཞག་པས།\
    \ སྐུ་ཉིད་ནས་ཨ་སྒོར་ ༩༩ གྲ་སྒྲིག་བྱས་ཏེ་ཀུ་ཤུའི་མཉེན་ཆས་ཁྲོམ་sdfs རའི་ཐོག་ཐོ་འགོད་དང་ནང་kgfg འཇུག་བྱས་ཏེ་\
    \རང་ཉིད་ལ་དབང་བའི་ས་མིག་ཟུངས་ཞེས་པ་རེད།"

counts :: [SpellItem] -> (Int, Int, Int, Int)
counts = foldl step (0, 0, 0, 0)
  where
    step (s, n, p, o) i = case i of
        Syllable _ -> (s + 1, n, p, o)
        Number _ -> (s, n + 1, p, o)
        Punct _ -> (s, n, p + 1, o)
        Other _ -> (s, n, p, o + 1)

longTextParses :: TestTree
longTextParses =
    testCase "long real text: every token preserved, no hang" $ do
        let toks = tokenizeUnicode longText
        case parseEither pSentence toks of
            Left e -> assertFailure (show e)
            Right items -> do
                rawsOf items @?= map tokenRaw toks
                counts items @?= (59, 1, 62, 2)

leadingShad :: TestTree
leadingShad =
    testCase "།དེ -> leading shad kept as Punct" $
        tagged (run "།དེ")
            @?= [ ("P", ["།"])
                , ("S", ["ད", "ེ"])
                ]

doubleShad :: TestTree
doubleShad =
    testCase "དེ།། -> consecutive shads grouped" $
        tagged (run "དེ།།")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", ["།", "།"])
                ]

newline :: TestTree
newline =
    testCase "དེ\nདུ -> newline is Other, keeps following syllable grouped (round-trip intact)" $
        tagged (run "དེ\nདུ")
            @?= [ ("S", ["ད", "ེ"])
                , ("O", ["\n", "ད", "ུ"])
                ]

digtsAfterSpace :: TestTree
digtsAfterSpace =
    testCase "སྒོ་ ༩༩ -> digits after space become Number" $
        tagged (run "སྒོ་ ༩༩")
            @?= [ ("S", ["ས", "ྒ", "ོ"])
                , ("P", ["་", " "])
                , ("N", ["༩", "༩"])
                ]

consecutivePunct :: TestTree
consecutivePunct =
    testCase "་་།། -> a run of punctuation stays one Punct item" $
        tagged (run "་་།།")
            @?= [ ("P", ["་", "་", "།", "།"])
                ]

allOther :: TestTree
allOther =
    testCase "abcde -> pure unknown input terminates as one Other" $
        tagged (run "abcde")
            @?= [ ("O", ["a", "b", "c", "d", "e"])
                ]