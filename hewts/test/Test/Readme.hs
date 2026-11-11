-- | The README transcript is part of the public contract: every @ghci>@ line
-- in @README.md@ is answered here from the library, so a changed API or a
-- stale example fails the suite instead of misleading a reader.
module Test.Readme (tests) where

import Control.Monad (forM)
import Convert
    ( InputFormat (..)
    , Settings (..)
    , checkSpelling
    , convertUnicodeToWylie
    , convertWylieToUnicode
    , defaultSettings
    , syllables
    )
import Convert.Diagnostic
    ( Diagnostic (..)
    , DiagnosticCode (..)
    , diagnosticList
    , renderDiagnostics
    )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "readme"
        [testCase "the transcript is the api's current output" check]

check :: Assertion
check = do
    readme <- TIO.readFile "README.md"
    let examples = transcript readme
    assertBool "README.md has no ```ghci transcript" (not (null examples))
    assertBool
        "README and Test.Readme list different examples, in a different order"
        (map fst examples == map fst answers)
    bad <- fmap concat $ forM examples $ \(expr, shown) ->
        pure $ case lookup expr answers of
            Nothing ->
                [problem "no answer in Test.Readme" expr [] shown]
            Just got
                | got == shown -> []
                | otherwise -> [problem "README and API disagree" expr got shown]
    assertBool ("\n" <> T.unpack (T.intercalate "\n\n" bad)) (null bad)
    where
        problem why expr got shown =
            why
                <> ":\nghci> "
                <> expr
                <> "\nREADME:\n"
                <> T.unlines shown
                <> "actual:\n"
                <> T.unlines got

-- | Every @ghci>@ line of every fenced @ghci@ block, with the non-blank lines
-- that follow it: what the reader sees printed. A statement (@:set@, @import@,
-- @let@) has no output of its own, so the next @ghci>@ line ends the answer.
transcript :: Text -> [(Text, [Text])]
transcript = concatMap block . ghciBlocks . T.lines
    where
        ghciBlocks [] = []
        ghciBlocks ("```ghci" : rest) =
            let (body, after) = break (== "```") rest
             in body : ghciBlocks (drop 1 after)
        ghciBlocks (_ : rest) = ghciBlocks rest

        block [] = []
        block (line : rest)
            | Just expr <- T.stripPrefix "ghci> " line =
                let (out, after) = span isOutput rest
                 in if null out then block after else (expr, out) : block after
            | otherwise = block rest

        isOutput line = not (T.null line) && not ("ghci> " `T.isPrefixOf` line)

-- | The answers the transcript documents, in the same spelling. A new example
-- in the README must be added here, and none of these may be left unused.
answers :: [(Text, [Text])]
answers =
    [ answer
        "either TIO.putStrLn (TIO.putStrLn . fst) (convertWylieToUnicode defaultSettings \"bkra shis\")"
        [either id fst (convertWylieToUnicode defaultSettings "bkra shis")]
    , answer
        "either TIO.putStrLn (TIO.putStrLn . fst) (convertUnicodeToWylie defaultSettings \"བཀྲ་ཤིས\")"
        [either id fst (convertUnicodeToWylie defaultSettings "བཀྲ་ཤིས")]
    , answer
        "print (fmap (map diagCode . diagnosticList) (checkSpelling wylie \"bk\"))"
        [shownCodes (checkSpelling wylie "bk")]
    , answer
        "either TIO.putStrLn (mapM_ TIO.putStrLn) (fmap (renderDiagnostics \"bk\") (checkSpelling wylie \"bk\"))"
        (either (: []) id (fmap (renderDiagnostics "bk") (checkSpelling wylie "bk")))
    , answer
        "either TIO.putStrLn (mapM_ TIO.putStrLn) (fmap (renderDiagnostics \"tgra\") (checkSpelling wylie \"tgra\"))"
        (either (: []) id (fmap (renderDiagnostics "tgra") (checkSpelling wylie "tgra")))
    , answer "print defaultSettings" [T.pack (show defaultSettings)]
    , answer
        "either TIO.putStrLn (print . map diagCode . diagnosticList . snd) (convertWylieToUnicode wylie \"tgra\")"
        (shownConvertCodes (convertWylieToUnicode wylie "tgra"))
    , answer
        "either TIO.putStrLn (print . map diagCode . diagnosticList . snd) (convertWylieToUnicode wylie{withChecks = False} \"tgra\")"
        (shownConvertCodes (convertWylieToUnicode wylie{withChecks = False} "tgra"))
    , answer
        "either TIO.putStrLn (TIO.putStrLn . fst) (convertWylieToUnicode wylie{sloppy = True} \" b m \")"
        [either id fst (convertWylieToUnicode wylie{sloppy = True} " b m ")]
    , answer
        "print (fmap (map diagCode . diagnosticList) (checkSpelling wylie \"rkwa\"))"
        [shownCodes (checkSpelling wylie "rkwa")]
    , answer
        "print (fmap (map diagCode . diagnosticList) (checkSpelling wylie{strict = True} \"rkwa\"))"
        [shownCodes (checkSpelling wylie{strict = True} "rkwa")]
    , answer
        "either TIO.putStrLn (mapM_ TIO.putStrLn) (syllables \"བཀྲ་ཤིས\")"
        (either (: []) id (syllables "བཀྲ་ཤིས"))
    ]
    where
        answer = (,)

        shownCodes = T.pack . show . fmap (map diagCode . diagnosticList)

        shownConvertCodes =
            either (const []) (pure . T.pack . show . map diagCode . diagnosticList . snd)

-- | Wylie with the input corrections off, as the README binds it.
wylie :: Settings
wylie = defaultSettings{inputFormat = InWylie}
