module Test.Convert (tests) where

import Convert (SpellItem (..), splitSentences, syllables)
import Convert.Token (tokenRaw)
import Data.Either (either)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
    testGroup
        "public Convert API"
        [ splitSentencesWorks
        , syllablesWorks
        , syllablesSkipNonSyllables
        , alwaysSuccess
        ]

run :: Text -> [SpellItem]
run = either (const []) id . splitSentences

tagged :: [SpellItem] -> [(Text, [Text])]
tagged = map $ \i -> case i of
    Syllable ts -> ("S", map (tokenRaw . snd) ts)
    Number ts -> ("N", map tokenRaw ts)
    Punct ts -> ("P", map tokenRaw ts)
    Other ts -> ("O", map tokenRaw ts)

splitSentencesWorks :: TestTree
splitSentencesWorks =
    testCase "splitSentences tags every item" $
        tagged (run "དེ་༡༢།")
            @?= [ ("S", ["ད", "ེ"])
                , ("P", ["་"])
                , ("N", ["༡", "༢"])
                , ("P", ["།"])
                ]

syllablesWorks :: TestTree
syllablesWorks =
    testCase "syllables extracts only syllable spellings" $
        syllables "མཆོག་དེ་རིང་ཕྱི་ཚེས་དུ་སུ་"
            @?= Right
                [ "མཆོག"
                , "དེ"
                , "རིང"
                , "ཕྱི"
                , "ཚེས"
                , "དུ"
                , "སུ"
                ]

syllablesSkipNonSyllables :: TestTree
syllablesSkipNonSyllables =
    testCase "syllables drops digits, punctuation and junk" $
        syllables "དེ་༡༢་xyz་དུ"
            @?= Right ["དེ", "དུ"]

alwaysSuccess :: TestTree
alwaysSuccess =
    testCase "empty input is a successful empty parse" $ do
        splitSentences "" @?= Right []
        syllables "" @?= Right []