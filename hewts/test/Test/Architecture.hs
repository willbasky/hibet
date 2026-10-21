-- | Layer checks of REFACTOR-AST §9.1, item 7. The plan separates the layers
-- (signs, reading, parse, syllable, printing); these tests keep a lower layer
-- from importing a higher one, so the separation is a fact of the code and not
-- only of the document.
module Test.Architecture (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "architecture"
        [ testCase "the syllable domain does not import the parser" $ do
            imports <- importsOf "src/Convert/Grammar/Syllable.hs"
            let leaks = importsOfModule "Convert.Grammar.Parser" imports
            assertBool ("Convert.Grammar.Syllable imports " <> show leaks) (null leaks)
        ]

importsOfModule :: String -> [String] -> [String]
importsOfModule moduleName = filter (moduleName `isInfixOf`)

importsOf :: FilePath -> IO [String]
importsOf path = do
    source <- TIO.readFile path
    pure
        [ T.unpack line
        | line <- T.lines source
        , "import" `T.isPrefixOf` T.stripStart line
        ]
