module Main where

import Label (LabelFull (..), Labels (..), Title (..), getLabels)
import Type (HibetError (..))

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    getLabelsSpec

getLabelsSpec :: Spec
getLabelsSpec =
    describe "getLabels" $ do
        it "Success with Test 1" $
            getLabels "titles.toml" toml `shouldBe` Right labels
        it "Rejects broken TOML as a value" $
            invalidPath (getLabels "titles.toml" "[[titles]")
                `shouldBe` Just "titles.toml"
        it "Rejects broken UTF-8 as a value" $
            invalidPath (getLabels "titles.toml" (BS.pack [0xff]))
                `shouldBe` Just "titles.toml"

invalidPath :: Either HibetError a -> Maybe FilePath
invalidPath = \case
    Left (DataInvalid path _) -> Just path
    _ -> Nothing

labels :: Labels
labels =
    Labels
        { labelTitles =
            [ LabelFull
                { path = "TsepakRigdzin-T|E"
                , lfId = 1
                , label = Title "Tsepak Rigdzin"
                , about =
                    "Tibetan-English Dictionary of Buddhist Terms|Revised and Enlarged Edition|Tsepak Rigdzin|Library of Tibetan Works and Archives|ISBN: 81-85102-88-0"
                , available = True
                , source = "Tibetan"
                , target = Set.fromList ["English"]
                , year = Nothing
                }
            , LabelFull
                { path = "Gaeng-Wetzel-T|ES"
                , lfId = 2
                , label = Title "G\228ng / Wetzel"
                , about =
                    "Buddhist Terms|Multilingual Version|Edited by Peter G\228ng and Sylvia Wetzel|Buddhist Academy Berlin Brandenburg|June 2004|Source:  http://www.buddhistische-akademie-bb.de/pdf/BuddhistTerms.pdf"
                , available = True
                , source = "Tibetan"
                , target = Set.fromList ["English", "Sanskrit"]
                , year = Nothing
                }
            ]
        }

toml :: BS.ByteString
toml =
    "[[titles]]\n    path = \"TsepakRigdzin-T|E\"\n    id = 1\n    label = \"Tsepak Rigdzin\"\n    highlight = \"\\\\[([^\\\\]]*)\\\\]\"\n    about = \"Tibetan-English Dictionary of Buddhist Terms|Revised and Enlarged Edition|Tsepak Rigdzin|Library of Tibetan Works and Archives|ISBN: 81-85102-88-0\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n\n[[titles]]\n    path = \"Gaeng-Wetzel-T|ES\"\n    id = 2\n    label = \"G\195\164ng / Wetzel\"\n    about = \"Buddhist Terms|Multilingual Version|Edited by Peter G\195\164ng and Sylvia Wetzel|Buddhist Academy Berlin Brandenburg|June 2004|Source:  http://www.buddhistische-akademie-bb.de/pdf/BuddhistTerms.pdf\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\", \"Sanskrit\"]\n"
