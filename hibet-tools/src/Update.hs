module Update
    ( Config (..)
    , Entry (..)
    , Result (..)
    , applyLocalFixes
    , defaultBaseUrl
    , defaultEntries
    , normalizeDictionary
    , runUpdate
    ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, forM, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Text.Normalize (NormalizationMode (NFC), normalize)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

data Entry = Entry
    { upstream :: !FilePath
    , local :: !FilePath
    }
    deriving stock (Eq, Show)

data Config = Config
    { dataRoot :: !FilePath
    , baseUrl :: !String
    , checkOnly :: !Bool
    , fetch :: String -> IO (Either String ByteString)
    }

data Result = Result
    { same :: !Int
    , changed :: !Int
    , added :: !Int
    , missingTitles :: ![FilePath]
    }
    deriving stock (Eq, Show)

defaultBaseUrl :: String -> String
defaultBaseUrl ref =
    "https://raw.githubusercontent.com/christiansteinert/tibetan-dictionary/"
        <> ref
        <> "/_input/dictionaries/public"

-- | Fetch and normalize every upstream file first, then compare and write. A
-- network failure leaves the existing dictionaries untouched.
runUpdate :: Config -> [Entry] -> IO (Either String Result)
runUpdate config entries = do
    downloaded <- forM entries $ \entry -> do
        fetched <- config.fetch (joinUrl config.baseUrl (upstream entry))
        pure $ case fetched of
            Left err -> Left ("FAIL: " <> upstream entry <> " (" <> err <> ")")
            Right bytes
                | BS.null bytes -> Left ("FAIL: empty " <> upstream entry)
                | otherwise -> case normalizeBytes (local entry) bytes of
                    Left err -> Left ("FAIL: " <> upstream entry <> " (" <> err <> ")")
                    Right text -> Right (entry, TE.encodeUtf8 text)
    case firstLeft downloaded of
        Just err -> pure (Left err)
        Nothing -> do
            let files = [pair | Right pair <- downloaded]
                dictsDir = dataRoot config </> "dicts"
            unless (checkOnly config) $
                createDirectoryIfMissing True dictsDir
            (nSame, nChanged, nAdded) <-
                foldM (compareEntry config dictsDir) (0, 0, 0) files
            titles <- try @IOException (TIO.readFile (dataRoot config </> "titles.toml"))
            pure $ case titles of
                Left err ->
                    Left
                        ( "cannot read "
                            <> (dataRoot config </> "titles.toml")
                            <> ": "
                            <> show err
                        )
                Right content ->
                    Right
                        Result
                            { same = nSame
                            , changed = nChanged
                            , added = nAdded
                            , missingTitles =
                                [ local entry
                                | entry <- entries
                                , not (hasTitle content (local entry))
                                ]
                            }

compareEntry ::
    Config
    -> FilePath
    -> (Int, Int, Int)
    -> (Entry, ByteString)
    -> IO (Int, Int, Int)
compareEntry config dictsDir (nSame, nChanged, nAdded) (entry, bytes) = do
    let dest = dictsDir </> (local entry <> ".txt")
    exists <- doesFileExist dest
    if exists
        then do
            old <- BS.readFile dest
            if old == bytes
                then pure (nSame + 1, nChanged, nAdded)
                else do
                    when (checkOnly config) $
                        putStrLn ("changed " <> local entry)
                    unless (checkOnly config) $
                        BS.writeFile dest bytes
                    pure (nSame, nChanged + 1, nAdded)
        else do
            when (checkOnly config) $
                putStrLn ("added   " <> local entry)
            unless (checkOnly config) $
                BS.writeFile dest bytes
            pure (nSame, nChanged, nAdded + 1)

hasTitle :: Text -> FilePath -> Bool
hasTitle content path =
    ("path = \"" <> T.pack path <> "\"") `T.isInfixOf` content

normalizeBytes :: FilePath -> ByteString -> Either String Text
normalizeBytes local bytes =
    case TE.decodeUtf8' bytes of
        Left err -> Left (show err)
        Right text -> Right (normalizeDictionary (applyLocalFixes local text))

-- | One entry per line, @upstream-name@ and the local stem used by the
-- dictionaries and @titles.toml@. The list mirrors the old bash script.
defaultEntries :: [Entry]
defaultEntries =
    [ Entry "01-Hopkins2015" "Hopkins-2015-T|E"
    , Entry "02-RangjungYeshe" "RangjungYeshe-T|E"
    , Entry "03-Berzin" "Berzin-T|E"
    , Entry "04-Berzin-Def" "Berzin-Def-T|E"
    , Entry "05-Hackett-Def2015" "Hackett-Def-2015-T|E"
    , Entry "05-Hopkins-Def2015" "Hopkins-Def-2015-T|E"
    , Entry "06-Hopkins-Comment" "Hopkins-Comment-T|E"
    , Entry "07-JimValby" "JimValby-T|E"
    , Entry "08-IvesWaldo" "IvesWaldo-T|E"
    , Entry "09-DanMartin" "DanMartin-T|TE"
    , Entry "10-RichardBarron" "RichardBarron-T|E"
    , Entry "11-Hopkins-Divisions2015" "Hopkins-Divisions-2015-T|ES"
    , Entry "12-Hopkins-DivisionsTib2015" "Hopkins-Divisions-2015-T|T"
    , Entry "13-Hopkins-Examples" "Hopkins-Examples-T|E"
    , Entry "14-Hopkins-ExamplesTib" "Hopkins-Examples-T|T"
    , Entry "15-Hopkins-Skt2015" "Hopkins-2015-T|S"
    , Entry "16-Hopkins-Synonyms1992" "Hopkins-Synonyms-T|E"
    , Entry "17-Hopkins-TibetanSynonyms1992" "Hopkins-Synonyms-T|T"
    , Entry "17-Hopkins-TibetanSynonyms2015" "Hopkins-Synonyms-2015-T|T"
    , Entry "18-Hopkins-TibetanDefinitions2015" "Hopkins-Def-2015-T|T"
    , Entry "19-Hopkins-TibetanTenses2015" "Hopkins-Tibetan-Tenses-2015-T|T"
    , Entry "20-Hopkins-othersEnglish2015" "Hopkins-others-T|E"
    , Entry "21-Mahavyutpatti-Skt" "Mahavyutpatti-T|S"
    , Entry "22-Yogacharabhumi-glossary" "Yoghacharabhumi-glossary-T|S"
    , Entry "23-GatewayToKnowledge" "Gateway-To-Knowledge-T|E"
    , Entry "25-tshig-mdzod-chen-mo-Tib" "tshig-mdzod-chen-mo-T|T"
    , Entry "26-Verbinator" "Verbinator-T|TE"
    , Entry "33-TsepakRigdzin" "TsepakRigdzin-T|E"
    , Entry "34-dung-dkar-tshig-mdzod-chen-mo-Tib" "dung-dkar-tshig-mdzod-chen-mo-T|T"
    , Entry "35-ThomasDoctor" "ThomasDoctor-T|E"
    , Entry "36-ComputerTerms" "Computer-Terms-T|E"
    , Entry "37-dag_tshig_gsar_bsgrigs-Tib" "dag_tshig_gsar_bsgrigs-T|T"
    , Entry "38-GaengWetzel" "Gaeng-Wetzel-T|ES"
    , Entry "40-CommonTerms-Lin" "CommonTerms-Lin-T|E"
    , Entry "42-Sera-Textbook-Definitions" "Sera-Textbook-Def-T|T"
    , Entry "43-84000Dict" "84000Dict-T|E"
    , Entry "44-84000Definitions" "84000Definitions-T|E"
    , Entry "45-84000Synonyms" "84000Synonyms-T|T"
    , Entry "46-84000Skt" "84000Skt-T|S"
    , Entry "47-Misc" "Misc-T|E"
    , Entry "48-TibTermProject" "TibTermProject-T|E"
    , Entry "49-LokeshChandraSkt" "LokeshChandraSkt-T|S"
    , Entry "50-NegiSkt" "NegiSkt-T|S"
    , Entry "51-LaineAbbreviations" "LaineAbbreviations-T|T"
    , Entry "52-ITLR" "ITLR-T|ES"
    , Entry "53-Bialek" "Bialek-T|E"
    , Entry
        "54-bod_rgya_nang_don_rig_pai_tshig_mdzod"
        "bod_rgya_nang_don_rig_pai_tshig_mdzod-T|T"
    , Entry "55-brda_dkrol_gser_gyi_me_long" "brda_dkrol_gser_gyi_me_long-T|T"
    , Entry "56-chos_rnam_kun_btus" "chos_rnam_kun_btus-T|T"
    , Entry "57-li_shii_gur_khang" "li_shii_gur_khang-T|T"
    , Entry "58-sgom_sde_tshig_mdzod_chen_mo" "sgom_sde_tshig_mdzod_chen_mo-T|T"
    , Entry
        "59-sgra_bye_brag_tu_rtogs_byed_chen_mo"
        "sgra_bye_brag_tu_rtogs_byed_chen_mo-T|ES"
    , Entry
        "60-sngas_rgyas_chos_gzhung_tshig_mdzod"
        "sngas_rgyas_chos_gzhung_tshig_mdzod-T|T"
    , Entry
        "61-gangs_can_mkhas_grub_rim_byon_ming_mdzod"
        "gangs_can_mkhas_grub_rim_byon_ming_mdzod-T|T"
    , Entry "62-bod_yig_tshig_gter_rgya_mtsho" "bod_yig_tshig_gter_rgya_mtsho-T|T"
    , Entry "67-hotl1" "hotl1-T|E"
    , Entry "67-hotl2" "hotl2-T|E"
    , Entry "67-hotl3" "hotl3-T|E"
    , Entry "68-tibetanlanguage-school" "tibetanlanguage-school-T|E"
    ]

-- | Normalize one upstream file: NFC, drop comments, blank lines, lines
-- without a separator and entries with an empty headword or definition.
normalizeDictionary :: Text -> Text
normalizeDictionary =
    T.unlines
        . mapMaybe normalizeLine
        . T.lines
    where
        normalizeLine line
            | "#" `T.isPrefixOf` line = Nothing
            | not ("|" `T.isInfixOf` line) = Nothing
            | otherwise =
                let line' = T.stripEnd line
                    (headword, rest) = T.breakOn "|" line'
                    definition = T.drop 1 rest
                 in if T.null (T.strip headword) || T.null definition
                        then Nothing
                        else Just (normalize NFC line')

-- | Keep two upstream regressions fixed locally. The old bash script did the
-- same with @sed@.
applyLocalFixes :: FilePath -> Text -> Text
applyLocalFixes local
    | local == "Sera-Textbook-Def-T|T" =
        T.replace "{yrongs grub" "{yongs grub"
    | local == "JimValby-T|E" =
        T.unlines . map fixJimValby . T.lines
    | otherwise = id
    where
        fixJimValby line
            | "sne btags|" `T.isPrefixOf` line
            , T.all isSpace (T.drop (T.length "sne btags|") line) =
                "sne btags|tag"
            | otherwise = line

joinUrl :: String -> FilePath -> String
joinUrl base path = dropWhileEnd (== '/') base <> "/" <> path

firstLeft :: [Either a b] -> Maybe a
firstLeft [] = Nothing
firstLeft (Left err : _) = Just err
firstLeft (Right _ : rest) = firstLeft rest
