module Main where

import Convert
    ( Settings (..)
    , convertWylieToUnicode
    , defaultSettings
    , syllables
    )
import Dictionary
import Env (Env (..), makeEnvFor, makeEnvFrom)
import Label (Title (..))
import Paths (testDir)
import Translator (getAnswer, tibetToWylieKey)
import Type (HibetError (..))

import qualified Data.HashMap.Strict as HMS
import Data.List (find, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    makeEnvFromSpec
    dictionarySearchSpec
    keyAliasSpec
    translate

dictionarySearchSpec :: Spec
dictionarySearchSpec =
    describe "Dictionary search" $ do
        it "finds a key" $
            search "re ba" "re ba|hope\n"
                `shouldBe` Just (Answer [Target "hope"] 1 (Title "test"))
        it "returns Nothing for an absent key" $
            search "me" "re ba|hope\n" `shouldBe` Nothing
        it "keeps every meaning of a duplicated key" $
            fmap
                (sortOn unTarget . targets)
                (search "re ba" "re ba|hope\nre ba|expectation\n")
                `shouldBe` Just [Target "expectation", Target "hope"]
        it "drops an exact duplicate line" $
            fmap targets (search "re ba" "re ba|hope\nre ba|hope\n")
                `shouldBe` Just [Target "hope"]
        it "treats an empty target list as no answer" $
            searchTranslation "re ba" (meta (HMS.fromList [("re ba", [])]))
                `shouldBe` Nothing
        it "adds the canonical spelling next to the legacy key" $ do
            let dict =
                    addAliases
                        (HMS.fromList [("bha ga", "b+ha ga")])
                        (makeDictionary "bha ga|bhaga\n")
                searchD q = fmap targets (searchTranslation q (meta dict))
            searchD "bha ga" `shouldBe` Just [Target "bhaga"]
            searchD "b+ha ga" `shouldBe` Just [Target "bhaga"]
        it "merges the articles when the canonical spelling is already a key" $ do
            let dict =
                    addAliases
                        (HMS.fromList [("gnad", "gand")])
                        (makeDictionary "gnad|pith\ngand|essence\n")
                searchD q =
                    fmap (sortOn unTarget . targets) (searchTranslation q (meta dict))
            searchD "gand" `shouldBe` Just [Target "essence", Target "pith"]
    where
        search query raw = searchTranslation query (meta (makeDictionary raw))
        meta dictionary = DictionaryMeta dictionary (Title "test") 1

makeEnvFromSpec :: Spec
makeEnvFromSpec =
    describe "Env loader" $ do
        it "loads titles and dictionaries from a directory" $ do
            res <- makeEnvFrom testDir []
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let meta = sortOn (\dm -> dm.number) env.dictionaryMeta
                    map (\dm -> dm.number) meta `shouldBe` [5, 7]
                    map (\dm -> dm.title) meta
                        `shouldBe` [Title "Hopkins", Title "Rangjung Yeshe"]
        it "adds the canonical spelling from key-aliases.tsv" $ do
            res <- makeEnvFrom testDir []
            case res of
                Left err -> expectationFailure $ show err
                Right env ->
                    case find (\dm -> dm.number == 5) env.dictionaryMeta of
                        Nothing -> expectationFailure "Hopkins dictionary is missing"
                        Just hopkins -> do
                            fmap targets (searchTranslation "bha ga" hopkins)
                                `shouldBe` Just [Target "bhaga"]
                            fmap targets (searchTranslation "b+ha ga" hopkins)
                                `shouldBe` Just [Target "bhaga"]

-- | The alias table is derived data. Every row must still name the key the
-- Unicode query path makes for the legacy spelling, and both spellings must
-- hold the same syllables; a stale row would silently add a wrong key.
keyAliasSpec :: Spec
keyAliasSpec =
    describe "Key aliases" $ do
        it "every row maps a legacy key to the canonical query key" $ do
            rows <- readAliasRows
            length rows `shouldSatisfy` (> 0)
            [ (legacy, canonical)
              | (legacy, canonical) <- rows
              , not (validRow legacy canonical)
              ]
                `shouldBe` []
    where
        validRow legacy canonical =
            canonical /= legacy
                && not (T.null canonical)
                && (tibetToWylieKey <$> tibetOf legacy) == Just (Right canonical)
                && (syllables <$> tibetOf legacy) == (syllables <$> tibetOf canonical)
        tibetOf key = case convertWylieToUnicode defaultSettings{withChecks = False} key of
            Left _ -> Nothing
            Right (tibet, _) -> Just tibet

readAliasRows :: IO [(Text, Text)]
readAliasRows = do
    text <- TIO.readFile "stuff/key-aliases.tsv"
    pure
        [ (legacy, T.drop 1 rest)
        | line <- T.lines text
        , let (legacy, rest) = T.breakOn "\t" line
        , not (T.null legacy)
        , not (T.null rest)
        ]

hasAnswers :: Either HibetError [Answer] -> Bool
hasAnswers (Right as) = not (null as)
hasAnswers _ = False

translate :: Spec
translate =
    describe "Translate Wylie request" $ do
        it "Search mir" $ do
            withEnv $ \env -> do
                let query = "mir"
                let reply = mapMaybe (searchTranslation query) env.dictionaryMeta
                sortOn dictNumber reply
                    `shouldBe` [ Answer
                                    { targets = [Target "termin. case of mi"]
                                    , dictNumber = 15
                                    , dictTitle = Title "James Valby"
                                    }
                               , Answer
                                    { targets = [Target "as/for/ to a human being"]
                                    , dictNumber = 16
                                    , dictTitle = Title "Ives Waldo"
                                    }
                               , Answer
                                    { targets =
                                        [ Target
                                            "{mi ru zhes pa'i ru yig bsdus pa ste/ srin po mir brdzus zhes pa lta bu/ (mir chags) ma'i mngal gyi byis pa/}"
                                        ]
                                    , dictNumber = 37
                                    , dictTitle = Title "dag-yig gsar-bsgrigs"
                                    }
                               ]
        it "Search ri ma" $ do
            withEnv $ \env -> do
                let reply = mapMaybe (searchTranslation "ri ma") env.dictionaryMeta
                reply
                    `shouldBe` [ Answer
                                    { targets = [Target "arid/ non-irrigated land"]
                                    , dictNumber = 16
                                    , dictTitle = Title "Ives Waldo"
                                    }
                               ]
        it "Search re ba byed pa" $ do
            withEnv $ \env -> do
                let reply = mapMaybe (searchTranslation "re ba byed pa") env.dictionaryMeta
                sortOn dictNumber reply
                    `shouldBe` [ Answer
                                    { targets = [Target "to hope, wish, expect, demand, ask"]
                                    , dictNumber = 7
                                    , dictTitle = Title "Rangjung Yeshe"
                                    }
                               , Answer
                                    { targets = [Target "demand, ask, hope, wish, expect"]
                                    , dictNumber = 15
                                    , dictTitle = Title "James Valby"
                                    }
                               ]
        it "Search re ba med pa" $ do
            withEnv $ \env -> do
                let reply = mapMaybe (searchTranslation "re ba med pa") env.dictionaryMeta
                sortOn dictNumber reply
                    `shouldBe` [ Answer
                                    { targets = [Target "hopeless; no hope"]
                                    , dictNumber = 5
                                    , dictTitle = Title "Hopkins"
                                    }
                               , Answer
                                    { targets = [Target "hopeless, despairing"]
                                    , dictNumber = 15
                                    , dictTitle = Title "James Valby"
                                    }
                               , Answer
                                    { targets = [Target "hopeless, no hope of --"]
                                    , dictNumber = 16
                                    , dictTitle = Title "Ives Waldo"
                                    }
                               , Answer
                                    { targets = [Target "[MSA]nirapekṣa; [MSA]niṣpratikāṅkṣa"]
                                    , dictNumber = 41
                                    , dictTitle = Title "Hopkins Sanskrit"
                                    }
                               ]
        it "Tibetan query reaches the same entries as its Wylie key" $ do
            withEnv $ \env -> do
                case (getAnswer "re ba med pa" env, getAnswer "རེ་བ་མེད་པ།" env) of
                    (Right (_, fromWylie), Right (_, fromTibet)) -> do
                        fromTibet `shouldBe` fromWylie
                        length fromTibet `shouldBe` 4
                    answers -> expectationFailure $ show answers
        it "Unicode query matrix reaches the same entries as its Wylie key" $ do
            withEnv $ \env -> do
                mapM_
                    (\(wylie, unicode) -> answersOf env unicode `shouldBe` answersOf env wylie)
                    unicodeMatrix
        it "Unicode reaches the canonical spelling of a legacy key" $ do
            withEnv $ \env -> do
                answersOf env "བྷ་ག" `shouldBe` answersOf env "b+ha ga"
                answersOf env "གནད" `shouldBe` answersOf env "gand"
                answersOf env "bha ga" `shouldSatisfy` hasAnswers
                answersOf env "gnad" `shouldSatisfy` hasAnswers
                case (answersOf env "bha ga", answersOf env "བྷ་ག") of
                    (Right legacy, Right unicode) ->
                        map dictNumber legacy
                            `shouldSatisfy` all (`elem` map dictNumber unicode)
                    other -> expectationFailure $ show other
                case (answersOf env "gnad", answersOf env "གནད") of
                    (Right legacy, Right unicode) ->
                        map dictNumber legacy
                            `shouldSatisfy` all (`elem` map dictNumber unicode)
                    other -> expectationFailure $ show other
        it "A malformed syllable does not resolve to the clean phrase" $ do
            withEnv $ \env -> do
                -- The broken run contributes no key, so the typo cannot pass
                -- for the phrase the user meant.
                answersOf env "ྐརེ་བ་མེད་པ" `shouldBe` Right []

answersOf :: Env -> Text -> Either HibetError [Answer]
answersOf env query = snd <$> getAnswer query env

-- | Every Unicode input stands for the Wylie key next to it. The cases are
-- the ways a query differs from a dictionary key: punctuation, spacing and
-- line breaks, foreign text (bare and bracketed), a lookalike Devanagari danda
-- instead of a shad, and digits.
unicodeMatrix :: [(Text, Text)]
unicodeMatrix =
    [ ("re ba med pa", "རེ་བ་མེད་པ།")
    , ("re ba med pa", "རེ་བ་ མེད་པ །")
    , ("re ba med pa", "རེ་བ་\nམེད་པ།")
    , ("re ba med pa", "རེ་བ་ hello མེད་པ།")
    , ("re ba med pa", "རེ་བ་ [hello] མེད་པ།")
    , ("re ba med pa", "རེ་ བ་ མེད་ པ")
    , ("re ba med pa", "རེ་བ་མེད་པ।")
    , ("re ba med pa", "རེ་བ་མེད་པ་༢༠༢༤")
    , ("re ba byed pa", "རེ་བ་བྱེད་པ།")
    , ("re ba", "རེ་བ།")
    , ("ri ma", "རི་མ།")
    , ("b+ha ga", "བྷ་ག")
    , ("gand", "གནད")
    ]

withEnv :: (Env -> IO ()) -> IO ()
withEnv action =
    makeEnvFor testDicts >>= either (fail . show) action

-- The expectations below are written for these dictionaries. New dictionaries
-- must not change the result of the search tests.
testDicts :: [Int]
testDicts = [5, 7, 15, 16, 37, 41]
