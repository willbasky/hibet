module Convert.Grammar where

import Data.Text (Text)
import Convert.Grammar.Common (Parser, pPunctuation, pNumber, recovering)
import qualified Convert.Grammar.Structures as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

p37structures :: Parser Text
p37structures = dbg "p37structures" $ do 
    space
    skipMany pPunctuation
    res <- recovering $ choice
        [ try S.pStructure1
        , try S.pStructure2
        , try S.pStructure3
        , try S.pStructure4
        , try S.pStructure5
        , try S.pStructure6
        , try S.pStructure7
        , try S.pStructure8
        , try S.pStructure9
        , try S.pStructure10
        , try S.pStructure11
        , try S.pStructure12
        , try S.pStructure13
        , try S.pStructure14
        , try S.pStructure15
        , try S.pStructure16
        , try S.pStructure17
        , try S.pStructure18
        , try S.pStructure19
        , try S.pStructure20
        , try S.pStructure21
        , try S.pStructure22
        , try S.pStructure23
        , try S.pStructure24
        , try S.pStructure25
        , try S.pStructure26
        , try S.pStructure27
        , try S.pStructure28
        , try S.pStructure29
        , try S.pStructure30
        , try S.pStructure31
        , try S.pStructure32
        , try S.pStructure33
        , try S.pStructure34
        , try S.pStructure35
        , try S.pStructure36
        , try S.pStructure37
        , pNumber
        ]
    space
    pure res

pSpellChekerUnicode :: Parser [Text] 
pSpellChekerUnicode = do 
    res <- some p37structures
    eof
    pure res

testText :: Text
testText = "མཆོག་དེ་རིང་ཕྱི་ཚེས་དུ་སུ་"

testLongText :: Text 
testLongText = "འོན་ཀྱང་དེ་རིང་ཉེ་སྔོན་གྱི་ལག་རྩལ་པས་ཡིག་ཟམ་ཞིག་བསྐུར་བར་ཀུ་ཤུས་རིན་མེད་ཞུབས་ཞུ་བྱེད་མཚམས་བཞག་པས། སྐུ་ཉིད་ནས་ཨ་སྒོར་ ༩༩ གྲ་སྒྲིག་བྱས་ཏེ་ཀུ་ཤུའི་མཉེན་ཆས་ཁྲོམ་sdfs རའི་ཐོག་ཐོ་འགོད་དང་ནང་kgfg འཇུག་བྱས་ཏེ་རང་ཉིད་ལ་དབང་བའི་ས་མིག་ཟུངས་ཞེས་པ་རེད།"

