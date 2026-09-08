module Test.Grammar.Rule.Structures (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule
  ( pStructure1
  , pStructure2
  , pStructure3
  , pStructure4
  , pStructure5
  , pStructure6
  , pStructure7
  , pStructure8
  )
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar structures"
    [ structure1
    , structure2
    , structure3
    , structure4
    , structure5
    , structure6
    , structure7
    , structure8
    ]

structure1 :: TestTree
structure1 =
  testGroup
    "structure 1"
    [ testCase "སཱ" $
        parseRaws pStructure1 "སཱ" @?= Right ["ས", "ཱ"]
    , testCase "ས" $
        parseRaws pStructure1 "ས" @?= Right ["ས"]
    , testCase "སོ" $
        parseRaws pStructure1 "སོ" @?= Right ["ས", "ོ"]
    , testCase "ཊཱ (no long vowel for sanskrit root, ཱ left unconsumed)" $
        parseRaws pStructure1 "ཊཱ" @?= Right ["ཊ"]
    , testCase "ཊོ" $
        parseRaws pStructure1 "ཊོ" @?= Right ["ཊ", "ོ"]
    , testCase "དུ་" $
        parseRaws pStructure1 "དུ་" @?= Right ["ད", "ུ"]
    , testCase "ཌ་" $
        parseRaws pStructure1 "ཌ་" @?= Right ["ཌ"]
    ]

structure2 :: TestTree
structure2 =
  testGroup
    "structure 2"
    [ testCase "སྒ" $
        parseRaws pStructure2 "སྒ" @?= Right ["ས", "ྒ"]
    , testCase "rejects plain root" $
        isLeft (parseRaws pStructure2 "ཀ") @?= True
    ]

structure3 :: TestTree
structure3 =
  testGroup
    "structure 3"
    [ testCase "སྲོ" $
        parseRaws pStructure3 "སྲོ" @?= Right ["ས", "ྲ", "ོ"]
    , testCase "སླ" $
        parseRaws pStructure3 "སླ" @?= Right ["ས", "ླ"]
    , testCase "rejects plain root" $
        isLeft (parseRaws pStructure3 "ཀ") @?= True
    ]

structure4 :: TestTree
structure4 =
  testGroup
    "structure 4"
    [ testCase "རྐྱུ" $
        parseRaws pStructure4 "རྐྱུ" @?= Right ["ར", "ྐ", "ྱ", "ུ"]
    , testCase "སྐྲོ" $
        parseRaws pStructure4 "སྐྲོ" @?= Right ["ས", "ྐ", "ྲ", "ོ"]
    , testCase "སྒྲ" $
        parseRaws pStructure4 "སྒྲ" @?= Right ["ས", "ྒ", "ྲ"]
    , testCase "སྤྱ" $
        parseRaws pStructure4 "སྤྱ" @?= Right ["ས", "ྤ", "ྱ"]
    , testCase "rejects plain root" $
        isLeft (parseRaws pStructure4 "ཀ") @?= True
    ]

structure5 :: TestTree
structure5 =
  testGroup
    "structure 5"
    [ testCase "བལྟ" $
        parseRaws pStructure5 "བལྟ" @?= Right ["བ", "ལ", "ྟ"]
    , testCase "བརྒ" $
        parseRaws pStructure5 "བརྒ" @?= Right ["བ", "ར", "ྒ"]
    , testCase "rejects plain root" $
        isLeft (parseRaws pStructure5 "ཀ") @?= True
    ]

structure6 :: TestTree
structure6 =
  testGroup
    "structure 6"
    [ testCase "འདྲ" $
        parseRaws pStructure6 "འདྲ" @?= Right ["འ", "ད", "ྲ"]
    , testCase "མགྱ" $
        parseRaws pStructure6 "མགྱ" @?= Right ["མ", "ག", "ྱ"]
    , testCase "མགྲ" $
        parseRaws pStructure6 "མགྲ" @?= Right ["མ", "ག", "ྲ"]
    , testCase "rejects plain root" $
        isLeft (parseRaws pStructure6 "ཀ") @?= True
    ]

structure7 :: TestTree
structure7 =
  testGroup
    "structure 7"
    [ testCase "བརྒྱ" $
        parseRaws pStructure7 "བརྒྱ" @?= Right ["བ", "ར", "ྒ", "ྱ"]
    , testCase "བསྒྲ" $
        parseRaws pStructure7 "བསྒྲ" @?= Right ["བ", "ས", "ྒ", "ྲ"]
    , testCase "rejects plain root" $
        isLeft (parseRaws pStructure7 "ཀ") @?= True
    ]

structure8 :: TestTree
structure8 =
  testGroup
    "structure 8"
    [ testCase "བཏ" $
        parseRaws pStructure8 "བཏ" @?= Right ["བ", "ཏ"]
    , testCase "གཏ" $
        parseRaws pStructure8 "གཏ" @?= Right ["ག", "ཏ"]
    , testCase "rejects root ང above prefix བ" $
        isLeft (parseRaws pStructure8 "བང") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)