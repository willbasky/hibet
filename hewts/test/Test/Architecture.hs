-- | Layer checks of REFACTOR-AST §9.1, item 7. The plan separates the layers
-- (signs, reading, parse, syllable, printing); these tests keep a lower layer
-- from importing a higher one and keep the renderer reading the syllable, not
-- the slice a sign was written as (§11.2). So the separation is a fact of the
-- code and not only of the document.
module Test.Architecture (tests) where

import Data.Char (isSpace)
import Data.List (isInfixOf, nub, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "architecture"
        [ testCase "the syllable domain does not import the parser" $ do
            imports <- importsOf "src/Convert/Grammar/Syllable.hs"
            let leaks = importsOfModule "Convert.Grammar.Parser" imports
            assertBool ("Convert.Grammar.Syllable imports " <> show leaks) (null leaks)
        , testCase "the notation does not import a constraint" $ do
            imports <- importsOf "src/Convert/Wylie/Tables.hs"
            let leaks = importsOfModule "Convert.Grammar.Constraint" imports
            assertBool ("Convert.Wylie.Tables imports " <> show leaks) (null leaks)
        , testCase "the renderer reads the sign only at the §11.2 places" $ do
            sites <- symbolSites "src/Convert.hs" (map fst renderExceptions)
            let seen symbol = nub (sort [decl | (found, decl) <- sites, found == symbol])
            [(symbol, seen symbol) | (symbol, _) <- renderExceptions] @?= renderExceptions
        ]

-- | The exceptions of REFACTOR-AST §11.2 and the `Convert.hs` declarations
-- each one lives in. They are permanent (Р7), not "until step N": each prints
-- a sign as it was written, and no step of the refactor changes that.
renderExceptions :: [(String, [String])]
renderExceptions =
    [ ("tokenRaw", ["renderItems"]) -- echo of an unplaced sign, orphan escapes
    , ("tokenSource", ["isUnicodeStack", "stackText"]) -- the escape branch, the a-chen
    , ("subjoinOf", []) -- closed by 6d: a subjoin prints from its canon
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

-- | Every place a module reads one of @symbols@, named by the top-level
-- declaration it sits in (a decl opens at column 0, a comment does not).
-- An import list names a function without reading it - that is how the
-- violation of 6d hid in another module - so imports are not sites.
symbolSites :: FilePath -> [String] -> IO [(String, String)]
symbolSites path symbols = do
    source <- TIO.readFile path
    pure
        [ (symbol, decl)
        | (decl, line) <- declarations (T.lines source)
        , not (null decl)
        , symbol <- symbols
        , symbol `isInfixOf` T.unpack line
        ]
    where
        declarations ls = go "" ls

        go _ [] = []
        go current (line : rest) =
            let decl = if isDeclaration line then name line else current
             in (decl, line) : go decl rest

        name = takeWhile (not . (`elem` (" :=(" :: String))) . T.unpack . T.stripStart

        isDeclaration line = case T.uncons line of
            Just (c, rest) ->
                not (isSpace c)
                    && not (T.null (T.strip rest))
                    && c /= '-'
                    && c /= '{'
                    && not ("import" `T.isPrefixOf` line)
                    && not ("module" `T.isPrefixOf` line)
            Nothing -> False
