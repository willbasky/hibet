-- | Layer checks of REFACTOR-AST §9.1, item 7. The plan separates the layers
-- (signs, reading, parse, syllable, printing); these tests keep a lower layer
-- from importing a higher one and keep the renderer reading the syllable, not
-- the slice a sign was written as (§11.2). So the separation is a fact of the
-- code and not only of the document.
--
-- The slice is the part of the input a sign was written with; the printing
-- layer may read it only where it echoes a sign nobody placed (§11.2). A
-- helper that reads the slice somewhere else and is handed to the printer is
-- the hole the review found once ('Token.subjoinOf', closed by 6d), so the
-- printers' import lists are pinned name by name: a new name in them is a
-- failed test, not a silent second way to decide a role (T2).
module Test.Architecture (tests) where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (isInfixOf, nub, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
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
            imports <- importsOf "src/Convert/Render/Wylie/Notation.hs"
            let leaks = importsOfModule "Convert.Grammar.Constraint" imports
            assertBool ("Convert.Render.Wylie.Notation imports " <> show leaks) (null leaks)
        , testCase "the renderer reads the sign only at the §11.2 places" $ do
            sites <-
                concat <$> traverse (`symbolSites` map fst renderExceptions) rendererFiles
            let seen symbol = nub (sort [decl | (found, decl) <- sites, found == symbol])
            [(symbol, seen symbol) | (symbol, _) <- renderExceptions] @?= renderExceptions
        , testCase "the notation never reads the sign" $ do
            sites <-
                symbolSites "src/Convert/Render/Wylie/Notation.hs" (map fst renderExceptions)
            sites @?= []
        , testCase "the printing layer imports only the recorded names" $ do
            let paths = [p | (p, _, _) <- printingModules]
            seen <- traverse importedNames paths
            let flatten ms = sort [n | (_, ns) <- ms, n <- ns]
                recorded = [(p, flatten ms) | (p, _, ms) <- printingModules]
            [(p, flatten ms) | (p, ms) <- zip paths seen] @?= recorded
        , testCase "the token layer does not import a higher layer" $ do
            leaks <-
                layerLeaks
                    ["src/Convert/Token.hs", "src/Convert/Token"]
                    ["Convert.Diagnostic", "Convert.Tokenizer", "Convert.Grammar", "Convert.Render"]
            leaks @?= []
        , testCase "the tokenizer does not import the grammar or printing" $ do
            leaks <-
                layerLeaks ["src/Convert/Tokenizer"] ["Convert.Grammar", "Convert.Render"]
            leaks @?= []
        , testCase "the grammar does not import reading or printing" $ do
            leaks <-
                layerLeaks ["src/Convert/Grammar"] ["Convert.Tokenizer", "Convert.Render"]
            leaks @?= []
        , testCase "the printing layer does not import reading or the parser" $ do
            leaks <-
                layerLeaks
                    ["src/Convert/Render"]
                    [ "Convert.Tokenizer"
                    , "Convert.Grammar.Parser"
                    , "Convert.Grammar.Constraint"
                    , "Convert.Grammar.Structure"
                    ]
            leaks @?= []
        ]

-- | The exceptions of REFACTOR-AST §11.2 and the declarations each one lives
-- in. They are permanent (Р7), not "until step N": each prints a sign as it
-- was written, and no step of the refactor changes that.
renderExceptions :: [(String, [String])]
renderExceptions =
    [
        ( "tokenRaw"
        ,
            [ "orphanEscapes"
            , "rawTokens"
            , "renderUnicodeItems"
            , "unknownText"
            , "wyliePiece" -- echo of an unplaced sign, orphan escapes
            ]
        )
    , ("tokenSource", ["isUnicodeStack", "stackText"]) -- the escape branch, the a-chen
    , ("subjoinOf", []) -- closed by 6d: a subjoin prints from its canon
    ]

-- | The modules that print signs: the Unicode arm, the Wylie arm and the
-- shared spelling of one sign. The notation is checked separately above.
rendererFiles :: [FilePath]
rendererFiles =
    [ "src/Convert/Render/Unicode.hs"
    , "src/Convert/Render/Wylie.hs"
    , "src/Convert/Render/Token.hs"
    ]

-- | What the printing layer may import, by module: the names it uses to spell
-- a syllable from its stacks and to echo the signs nobody placed. Pin them
-- here, with a reason where the name is not obvious, so that a new import is
-- a decision made in the open.
printingModules :: [(FilePath, String, [(String, [String])])]
printingModules =
    [
        ( "src/Convert/Render.hs"
        , "Convert.Render"
        ,
            [ ("Convert.Grammar.Sentence", ["SpellItem"])
            , ("Convert.Render.Unicode", ["renderUnicodeItems"])
            , ("Convert.Render.Wylie", ["renderWylie"])
            , ("Data.Text", ["Text"])
            ]
        )
    ,
        ( "src/Convert/Render/Token.hs"
        , "Convert.Render.Token"
        ,
            [
                ( "Convert.Token"
                , ["Token", "TokenCanonical", "UnknownMark", "tokenCanonical", "tokenRaw"]
                )
            , ("Data.Maybe", ["fromMaybe"])
            , ("Data.Text", ["Text"])
            ]
        )
    ,
        ( "src/Convert/Render/Unicode.hs"
        , "Convert.Render.Unicode"
        ,
            [ ("Convert.Grammar.Sentence", ["SpellItem"])
            ,
                ( "Convert.Grammar.Syllable"
                , ["Stack", "Subfixes", "Syllable", "VowelSlot", "finalsTokens", "stackTokens"]
                )
            , ("Convert.Render.Token", ["spellToken"])
            ,
                ( "Convert.Token"
                ,
                    [ "Token"
                    , "TokenCanonical"
                    , "TokenSource"
                    , "toSubjoined"
                    , "tokenCanonical"
                    , "tokenRaw" -- §11.2: the echo of an unplaced sign
                    , "tokenSource" -- §11.2: the escape branch and the a-chen
                    ]
                )
            , ("Convert.Token.Unicode", ["unicodeOf"])
            , ("Data.Foldable", ["toList"])
            , ("Data.Maybe", ["fromMaybe"])
            , ("Data.Text", ["Text"])
            ]
        )
    ,
        ( "src/Convert/Render/Wylie.hs"
        , "Convert.Render.Wylie"
        ,
            [ ("Convert.Grammar.Sentence", ["SpellItem"])
            , ("Convert.Grammar.Syllable", ["Syllable", "stackTokens"])
            , ("Convert.Render.Token", ["spellToken", "unknownText"])
            , ("Convert.Render.Unicode", ["isOrphanStack", "isUnicodeStack"])
            , ("Convert.Render.Wylie.Notation", ["Join", "notate"])
            ,
                ( "Convert.Token"
                ,
                    [ "SpaceMark"
                    , "Token"
                    , "TokenCanonical"
                    , "UnknownMark"
                    , "isToplessMark"
                    , "tokenCanonical"
                    , "tokenRaw" -- §11.2: the echo of an unplaced sign
                    ]
                )
            , ("Convert.Token.Unicode", ["unicodeOf"])
            , ("Convert.Token.Wylie", ["wylieOf"])
            , ("Data.Char", ["ord"])
            , ("Data.Foldable", ["toList"])
            , ("Data.Text", ["Text"])
            , ("Numeric", ["showHex"])
            ]
        )
    ,
        ( "src/Convert/Render/Wylie/Notation.hs"
        , "Convert.Render.Wylie.Notation"
        ,
            [
                ( "Convert.Grammar.Superfix"
                ,
                    [ "lSuperfixRoots"
                    , "rSuperfixRoots"
                    , "rootSubfixRows"
                    , "sSuperfixRoots"
                    , "superfixCombos"
                    ]
                )
            ,
                ( "Convert.Grammar.Syllable"
                , ["Role", "Stack", "Subfixes", "Syllable", "VowelSlot", "finalsTokens"]
                )
            ,
                ( "Convert.Token"
                , ["Consonant", "FinalMark", "Token", "TokenCanonical", "tokenCanonical"]
                )
            , ("Convert.Token.Wylie", ["wylieExpansions", "wylieOf"])
            , ("Data.Foldable", ["toList"])
            , ("Data.Maybe", ["fromMaybe"])
            , ("Data.Sequence", ["Seq"])
            , ("Data.Set", ["Set"])
            , ("Data.Text", ["Text"])
            ]
        )
    ]

-- | Every import of a layer that names a module it must not know about, by
-- file. The roots may be files or directories: a directory is walked, so the
-- check follows the tree instead of a hand-written list.
layerLeaks :: [FilePath] -> [String] -> IO [(FilePath, [String])]
layerLeaks roots forbidden = do
    files <- concat <$> traverse hsFiles roots
    fmap concat $ forM files $ \path -> do
        imports <- importsOf path
        let leaks = [line | line <- imports, any (`isInfixOf` line) forbidden]
        pure [(path, leaks) | not (null leaks)]

hsFiles :: FilePath -> IO [FilePath]
hsFiles path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            entries <- listDirectory path
            concat <$> traverse (hsFiles . (path </>)) entries
        else pure [path | takeExtension path == ".hs"]

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

-- | The names a module imports by name, module by module. An import with no
-- list (or a qualified one with no list) brings in nothing by name; the names
-- inside a list lose their constructors: @Stack (..)@ is @Stack@.
importedNames :: FilePath -> IO [(String, [String])]
importedNames path = do
    source <- TIO.readFile path
    pure (map names (importBlocks (T.lines source)))
    where
        names chunk = (moduleOf chunk, entries chunk)

        moduleOf chunk =
            case filter (/= "qualified") (T.words (T.stripStart chunk)) of
                m : _ -> T.unpack m
                [] -> ""

        entries chunk = case T.breakOn "(" chunk of
            (_, rest)
                | T.null rest -> []
                | otherwise ->
                    map (T.unpack . T.takeWhile (not . isSpace) . T.strip) $
                        T.splitOn "," (T.drop 1 (T.dropWhileEnd (== ')') rest))

-- | The import statements of a file, each one as a single line: a statement
-- starts at column 0 with @import@ and goes on while the next line is blank in
-- its first column (the indented list of names).
importBlocks :: [T.Text] -> [T.Text]
importBlocks = go
    where
        go [] = []
        go (line : rest)
            | "import" `T.isPrefixOf` line =
                let (more, after) = span continuation rest
                 in T.unwords (line : more) : go after
            | otherwise = go rest

        continuation line = case T.uncons line of
            Just (c, _) -> isSpace c && not (T.null (T.strip line))
            Nothing -> False

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
