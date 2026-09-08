module Test.Grammar.Sentence (tests) where

import Convert.Grammar.Parser (parseEither)
import Convert.Grammar.Sentence (SpellItem (..), pSentence)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (either)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

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
        ]

itemRaws :: SpellItem -> [Text]
itemRaws (Syllable ts) = map tokenRaw ts
itemRaws (Punct ts) = map tokenRaw ts
itemRaws (Other ts) = map tokenRaw ts

tagged :: [SpellItem] -> [(Text, [Text])]
tagged = map $ \i -> case i of
    Syllable ts -> ("S", map tokenRaw ts)
    Punct ts -> ("P", map tokenRaw ts)
    Other ts -> ("O", map tokenRaw ts)

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
    testCase "དེ་༡༢་ -> number tokens as Other" $
        tagged (run "དེ་༡༢་")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", ["་"])
                , ("O", ["༡", "༢"])
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
    rawsOf = concatMap itemRaws