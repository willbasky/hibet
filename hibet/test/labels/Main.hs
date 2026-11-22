module Main where

import Io (Utf8Error (..), readUtf8, writeUtf8)
import Label (LabelFull (..), Labels (..), Title (..), getLabels)
import Type (HibetError (..))

import Control.Exception (finally)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.IO.Encoding (getLocaleEncoding, latin1, setLocaleEncoding)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , hspec
    , it
    , shouldBe
    , shouldSatisfy
    )

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
        it "readUtf8 rejects broken UTF-8 with the path" $ do
            result <- readUtf8 "test/env/data/bad-utf8/titles.toml"
            result `shouldSatisfy` \case
                Left (InvalidUtf8 path _) ->
                    path == "test/env/data/bad-utf8/titles.toml"
                _ -> False
        it "reads and writes UTF-8 regardless of the locale encoding" $
            withSystemTempDirectory "hibet-io" $ \dir -> do
                let path = dir <> "/tib.txt"
                old <- getLocaleEncoding
                flip finally (setLocaleEncoding old) $ do
                    setLocaleEncoding latin1
                    writeUtf8 path "ཀ"
                    readUtf8 path >>= either (expectationFailure . show) (`shouldBe` "ཀ")

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

toml :: Text
toml =
    "[[titles]]\n    path = \"TsepakRigdzin-T|E\"\n    id = 1\n    label = \"Tsepak Rigdzin\"\n    highlight = \"\\\\[([^\\\\]]*)\\\\]\"\n    about = \"Tibetan-English Dictionary of Buddhist Terms|Revised and Enlarged Edition|Tsepak Rigdzin|Library of Tibetan Works and Archives|ISBN: 81-85102-88-0\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n\n[[titles]]\n    path = \"Gaeng-Wetzel-T|ES\"\n    id = 2\n    label = \"G\228ng / Wetzel\"\n    about = \"Buddhist Terms|Multilingual Version|Edited by Peter G\228ng and Sylvia Wetzel|Buddhist Academy Berlin Brandenburg|June 2004|Source:  http://www.buddhistische-akademie-bb.de/pdf/BuddhistTerms.pdf\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\", \"Sanskrit\"]\n"
