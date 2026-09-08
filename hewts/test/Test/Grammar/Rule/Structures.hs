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
  , pStructure9
  , pStructure10
  , pStructure11
  , pStructure12
  , pStructure13
  , pStructure14
  , pStructure15
  , pStructure16
  , pStructure17
  , pStructure18
  , pStructure19
  , pStructure20
  , pStructure21
  , pStructure22
  , pStructure23
  , pStructure24
  , pStructure25
  , pStructure26
  , pStructure27
  , pStructure28
  , pStructure29
  , pStructure30
  , pStructure31
  , pStructure32
  , pStructure33
  , pStructure34
  , pStructure35
  , pStructure36
  , pStructure37
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
    , structure9
    , structure10
    , structure11
    , structure12
    , structure13
    , structure14
    , structure15
    , structure16
    , structure17
    , structure18
    , structure19
    , structure20
    , structure21
    , structure22
    , structure23
    , structure24
    , structure25
    , structure26
    , structure27
    , structure28
    , structure29
    , structure30
    , structure31
    , structure32
    , structure33
    , structure34
    , structure35
    , structure36
    , structure37
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

structure9 :: TestTree
structure9 =
  testGroup
    "structure 9"
    [ testCase "བཏག" $
        parseRaws pStructure9 "བཏག" @?= Right ["བ", "ཏ", "ག"]
    , testCase "བཏིག" $
        parseRaws pStructure9 "བཏིག" @?= Right ["བ", "ཏ", "ི", "ག"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure9 "བཏ") @?= True
    ]

structure10 :: TestTree
structure10 =
  testGroup
    "structure 10"
    [ testCase "བརྒན" $
        parseRaws pStructure10 "བརྒན" @?= Right ["བ", "ར", "ྒ", "ན"]
    , testCase "བརྒིག" $
        parseRaws pStructure10 "བརྒིག" @?= Right ["བ", "ར", "ྒ", "ི", "ག"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure10 "བརྒ") @?= True
    ]

structure11 :: TestTree
structure11 =
  testGroup
    "structure 11"
    [ testCase "མགྱས" $
        parseRaws pStructure11 "མགྱས" @?= Right ["མ", "ག", "ྱ", "ས"]
    , testCase "མགྲིས" $
        parseRaws pStructure11 "མགྲིས" @?= Right ["མ", "ག", "ྲ", "ི", "ས"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure11 "མགྲ") @?= True
    ]

structure12 :: TestTree
structure12 =
  testGroup
    "structure 12"
    [ testCase "བརྒྱས" $
        parseRaws pStructure12 "བརྒྱས" @?= Right ["བ", "ར", "ྒ", "ྱ", "ས"]
    , testCase "བསྒྲས" $
        parseRaws pStructure12 "བསྒྲས" @?= Right ["བ", "ས", "ྒ", "ྲ", "ས"]
    , testCase "བསྒྲོས" $
        parseRaws pStructure12 "བསྒྲོས" @?= Right ["བ", "ས", "ྒ", "ྲ", "ོ", "ས"]
    , testCase "བསྒྱས" $
        parseRaws pStructure12 "བསྒྱས" @?= Right ["བ", "ས", "ྒ", "ྱ", "ས"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure12 "བསྒྲ") @?= True
    ]

structure13 :: TestTree
structure13 =
  testGroup
    "structure 13"
    [ testCase "བཏནད" $
        parseRaws pStructure13 "བཏནད" @?= Right ["བ", "ཏ", "ན", "ད"]
    , testCase "གཏིགས" $
        parseRaws pStructure13 "གཏིགས" @?= Right ["ག", "ཏ", "ི", "ག", "ས"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure13 "བཏན") @?= True
    ]

structure14 :: TestTree
structure14 =
  testGroup
    "structure 14"
    [ testCase "བརྟགས" $
        parseRaws pStructure14 "བརྟགས" @?= Right ["བ", "ར", "ྟ", "ག", "ས"]
    , testCase "བརྟིབས" $
        parseRaws pStructure14 "བརྟིབས" @?= Right ["བ", "ར", "ྟ", "ི", "བ", "ས"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure14 "བརྟག") @?= True
    ]

structure15 :: TestTree
structure15 =
  testGroup
    "structure 15"
    [ testCase "དགྱིགས" $
        parseRaws pStructure15 "དགྱིགས" @?= Right ["ད", "ག", "ྱ", "ི", "ག", "ས"]
    , testCase "དགྲོགས" $
        parseRaws pStructure15 "དགྲོགས" @?= Right ["ད", "ག", "ྲ", "ོ", "ག", "ས"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure15 "དགྱིག") @?= True
    ]

structure16 :: TestTree
structure16 =
  testGroup
    "structure 16"
    [ testCase "བསྒྱུགས" $
        parseRaws pStructure16 "བསྒྱུགས" @?= Right ["བ", "ས", "ྒ", "ྱ", "ུ", "ག", "ས"]
    , testCase "བརྐྱནད" $
        parseRaws pStructure16 "བརྐྱནད" @?= Right ["བ", "ར", "ྐ", "ྱ", "ན", "ད"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure16 "བསྒྱུག") @?= True
    ]

structure17 :: TestTree
structure17 =
  testGroup
    "structure 17"
    [ testCase "བིག" $
        parseRaws pStructure17 "བིག" @?= Right ["བ", "ི", "ག"]
    , testCase "བས" $
        parseRaws pStructure17 "བས" @?= Right ["བ", "ས"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure17 "བི") @?= True
    ]

structure18 :: TestTree
structure18 =
  testGroup
    "structure 18"
    [ testCase "རྒིད" $
        parseRaws pStructure18 "རྒིད" @?= Right ["ར", "ྒ", "ི", "ད"]
    , testCase "སྒོལ" $
        parseRaws pStructure18 "སྒོལ" @?= Right ["ས", "ྒ", "ོ", "ལ"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure18 "སྒོ") @?= True
    ]

structure19 :: TestTree
structure19 =
  testGroup
    "structure 19"
    [ testCase "ཁྱོའ" $
        parseRaws pStructure19 "ཁྱོའ" @?= Right ["ཁ", "ྱ", "ོ", "འ"]
    , testCase "བླིན" $
        parseRaws pStructure19 "བླིན" @?= Right ["བ", "ླ", "ི", "ན"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure19 "ཁྱོ") @?= True
    ]

structure20 :: TestTree
structure20 =
  testGroup
    "structure 20"
    [ testCase "སྐྲོན" $
        parseRaws pStructure20 "སྐྲོན" @?= Right ["ས", "ྐ", "ྲ", "ོ", "ན"]
    , testCase "སྒྱར" $
        parseRaws pStructure20 "སྒྱར" @?= Right ["ས", "ྒ", "ྱ", "ར"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure20 "སྐྲོ") @?= True
    ]

structure21 :: TestTree
structure21 =
  testGroup
    "structure 21"
    [ testCase "བགས" $
        parseRaws pStructure21 "བགས" @?= Right ["བ", "ག", "ས"]
    , testCase "པོགས" $
        parseRaws pStructure21 "པོགས" @?= Right ["པ", "ོ", "ག", "ས"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure21 "པོག") @?= True
    ]

structure22 :: TestTree
structure22 =
  testGroup
    "structure 22"
    [ testCase "རྨགས" $
        parseRaws pStructure22 "རྨགས" @?= Right ["ར", "ྨ", "ག", "ས"]
    , testCase "སྣོབས" $
        parseRaws pStructure22 "སྣོབས" @?= Right ["ས", "ྣ", "ོ", "བ", "ས"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure22 "རྨག") @?= True
    ]

structure23 :: TestTree
structure23 =
  testGroup
    "structure 23"
    [ testCase "རློམས" $
        parseRaws pStructure23 "རློམས" @?= Right ["ར", "ླ", "ོ", "མ", "ས"]
    , testCase "དྭོལད" $
        parseRaws pStructure23 "དྭོལད" @?= Right ["ད", "ྭ", "ོ", "ལ", "ད"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure23 "རློམ") @?= True
    ]

structure24 :: TestTree
structure24 =
  testGroup
    "structure 24"
    [ testCase "རྩྭོངས" $
        parseRaws pStructure24 "རྩྭོངས" @?= Right ["ར", "ྩ", "ྭ", "ོ", "ང", "ས"]
    , testCase "སྣྲེནད" $
        parseRaws pStructure24 "སྣྲེནད" @?= Right ["ས", "ྣ", "ྲ", "ེ", "ན", "ད"]
    , testCase "rejects missing postfix" $
        isLeft (parseRaws pStructure24 "རྩྭོང") @?= True
    ]

structure25 :: TestTree
structure25 =
  testGroup
    "structure 25"
    [ testCase "དྲྭ" $
        parseRaws pStructure25 "དྲྭ" @?= Right ["ད", "ྲ", "ྭ"]
    , testCase "ཕྱྭ" $
        parseRaws pStructure25 "ཕྱྭ" @?= Right ["ཕ", "ྱ", "ྭ"]
    , testCase "rejects subfix without ra/ya root" $
        isLeft (parseRaws pStructure25 "དྭ") @?= True
    ]

structure26 :: TestTree
structure26 =
  testGroup
    "structure 26"
    [ testCase "ཧྥ" $
        parseRaws pStructure26 "ཧྥ" @?= Right ["ཧ", "ྥ"]
    , testCase "ཧྥོ" $
        parseRaws pStructure26 "ཧྥོ" @?= Right ["ཧ", "ྥ", "ོ"]
    , testCase "rejects wrong subroot" $
        isLeft (parseRaws pStructure26 "ཧྲ") @?= True
    ]

structure27 :: TestTree
structure27 =
  testGroup
    "structure 27"
    [ testCase "ཧྥེལ" $
        parseRaws pStructure27 "ཧྥེལ" @?= Right ["ཧ", "ྥ", "ེ", "ལ"]
    , testCase "ཧྥོས" $
        parseRaws pStructure27 "ཧྥོས" @?= Right ["ཧ", "ྥ", "ོ", "ས"]
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pStructure27 "ཧྥ") @?= True
    ]

structure28 :: TestTree
structure28 =
  testGroup
    "structure 28"
    [ testCase "ཧིའེ" $
        parseRaws pStructure28 "ཧིའེ" @?= Right ["ཧ", "ི", "འ", "ེ"]
    , testCase "གོའྨ" $
        parseRaws pStructure28 "གོའྨ" @?= Right ["ག", "ོ", "འ", "ྨ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure28 "ཧི") @?= True
    ]

structure29 :: TestTree
structure29 =
  testGroup
    "structure 29"
    [ testCase "སྨོའྨ" $
        parseRaws pStructure29 "སྨོའྨ" @?= Right ["ས", "ྨ", "ོ", "འ", "ྨ"]
    , testCase "རྒའོ" $
        parseRaws pStructure29 "རྒའོ" @?= Right ["ར", "ྒ", "འ", "ོ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure29 "སྨོ") @?= True
    ]

structure30 :: TestTree
structure30 =
  testGroup
    "structure 30"
    [ testCase "སླིའྔ" $
        parseRaws pStructure30 "སླིའྔ" @?= Right ["ས", "ླ", "ི", "འ", "ྔ"]
    , testCase "ཀྭོའོ" $
        parseRaws pStructure30 "ཀྭོའོ" @?= Right ["ཀ", "ྭ", "ོ", "འ", "ོ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure30 "སླི") @?= True
    ]

structure31 :: TestTree
structure31 =
  testGroup
    "structure 31"
    [ testCase "སྒྱའོ" $
        parseRaws pStructure31 "སྒྱའོ" @?= Right ["ས", "ྒ", "ྱ", "འ", "ོ"]
    , testCase "རྐྱའོ" $
        parseRaws pStructure31 "རྐྱའོ" @?= Right ["ར", "ྐ", "ྱ", "འ", "ོ"]
    , testCase "རྐྱའ" $
        parseRaws pStructure31 "རྐྱའ" @?= Right ["ར", "ྐ", "ྱ", "འ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure31 "སྒྱ") @?= True
    ]

structure32 :: TestTree
structure32 =
  testGroup
    "structure 32"
    [ testCase "བལྟའ" $
        parseRaws pStructure32 "བལྟའ" @?= Right ["བ", "ལ", "ྟ", "འ"]
    , testCase "བརྔོའ" $
        parseRaws pStructure32 "བརྔོའ" @?= Right ["བ", "ར", "ྔ", "ོ", "འ"]
    , testCase "བསྟིའི" $
        parseRaws pStructure32 "བསྟིའི" @?= Right ["བ", "ས", "ྟ", "ི", "འ", "ི"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure32 "བལྟ") @?= True
    ]

structure33 :: TestTree
structure33 =
  testGroup
    "structure 33"
    [ testCase "མཁྲའ" $
        parseRaws pStructure33 "མཁྲའ" @?= Right ["མ", "ཁ", "ྲ", "འ"]
    , testCase "དཔྱུའ" $
        parseRaws pStructure33 "དཔྱུའ" @?= Right ["ད", "པ", "ྱ", "ུ", "འ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure33 "མཁྲ") @?= True
    ]

structure34 :: TestTree
structure34 =
  testGroup
    "structure 34"
    [ testCase "བསྐྲའ" $
        parseRaws pStructure34 "བསྐྲའ" @?= Right ["བ", "ས", "ྐ", "ྲ", "འ"]
    , testCase "བརྒྱའ" $
        parseRaws pStructure34 "བརྒྱའ" @?= Right ["བ", "ར", "ྒ", "ྱ", "འ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure34 "བསྐྲ") @?= True
    ]

structure35 :: TestTree
structure35 =
  testGroup
    "structure 35"
    [ testCase "བཟིའ" $
        parseRaws pStructure35 "བཟིའ" @?= Right ["བ", "ཟ", "ི", "འ"]
    , testCase "གཤའ" $
        parseRaws pStructure35 "གཤའ" @?= Right ["ག", "ཤ", "འ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure35 "བཟ") @?= True
    ]

structure36 :: TestTree
structure36 =
  testGroup
    "structure 36"
    [ testCase "གྲྭའ" $
        parseRaws pStructure36 "གྲྭའ" @?= Right ["ག", "ྲ", "ྭ", "འ"]
    , testCase "ཕྱྭུའ" $
        parseRaws pStructure36 "ཕྱྭུའ" @?= Right ["ཕ", "ྱ", "ྭ", "ུ", "འ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure36 "གྲྭ") @?= True
    ]

structure37 :: TestTree
structure37 =
  testGroup
    "structure 37"
    [ testCase "ཧྥོའ" $
        parseRaws pStructure37 "ཧྥོའ" @?= Right ["ཧ", "ྥ", "ོ", "འ"]
    , testCase "ཧྥའ" $
        parseRaws pStructure37 "ཧྥའ" @?= Right ["ཧ", "ྥ", "འ"]
    , testCase "rejects missing a-chung chain" $
        isLeft (parseRaws pStructure37 "ཧྥ") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)