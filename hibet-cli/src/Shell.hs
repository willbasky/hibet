module Shell
    ( mkOutput
    , queryScale
    , translator
    )
where

import Dictionary (Answer, sortOutput)
import Env (Env (..), readEnv)
import Pretty
    ( Line (NewLine)
    , blue
    , pprint
    , putColorDoc
    , red
    , sized
    , supportsTextSizing
    , terminalWidth
    , viewTranslations
    , withHeaderSpaces
    , yellow
    )
import Translator (getAnswer)

import Control.Concurrent.MVar (MVar)
import Control.Exception (bracketOnError)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.Console.Haskeline as Haskeline
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)
import qualified System.Console.Haskeline.IO as Haskeline
import System.Exit (exitSuccess)

-- | Start the interactive translator over an environment that is loaded in
-- the background.
translator :: MVar Env -> IO ()
translator mv = bracketOnError
    (Haskeline.initializeInput Haskeline.defaultSettings)
    Haskeline.cancelInput -- This will only be called if an exception such as a SigINT is received.
    $ \inputState -> do
        putColorDoc
            blue
            NewLine
            "Input a word in the tibetan script or wylie transcription:"
        loopDialog mv inputState
        Haskeline.closeInput inputState

-- Looped dialog with user
loopDialog ::
    MVar Env
    -> InputState
    -> IO ()
-- Not 'forever': at EOF 'getInput' keeps returning 'Nothing', so 'forever'
-- would spin on the closed input. Returning lets 'translator' call 'closeInput'.
loopDialog mv inputState = do
    mQuery <- Haskeline.queryInput inputState $ Haskeline.getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just "" -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "ཞེས་བསྟན་འཛིན་བཟང་པོ། Bye!"
            exitSuccess
        Just ":h" -> do
            history <-
                fromHistory
                    <$> Haskeline.queryInput inputState Haskeline.getHistory
            mapM_ (putColorDoc id NewLine) history
        Just input -> do
            env <- readEnv mv
            if null env.dictionaryMeta
                then
                    putColorDoc
                        red
                        NewLine
                        "No dictionaries are selected. Run 'hibet show names' to list ids."
                else case getAnswer input env of
                    Left _ -> do
                        putColorDoc red NewLine "Nothing found"
                    Right (query, answer) ->
                        if null answer
                            then putColorDoc red NewLine "Nothing found"
                            else do
                                width <- terminalWidth
                                scalable <- supportsTextSizing
                                let scale = if scalable then queryScale width query else 1
                                pprint
                                    (Haskeline.queryInput inputState (Haskeline.getInputChar ""))
                                    $ mkOutput scale query answer
    case mQuery of
        Nothing -> pure ()
        Just _ -> loopDialog mv inputState

fromHistory :: History -> [Text]
fromHistory =
    reverse
        . map T.pack
        . filter (/= ":h")
        . historyLines

mkOutput :: Int -> Text -> [Answer] -> Doc AnsiStyle
mkOutput scale query answers =
    withHeaderSpaces yellow (if scale > 1 then sized scale query else query)
        $ viewTranslations
        $ sortOutput answers

-- | Largest scale from 1 to 4 that keeps the query inside the terminal, less
-- the 2-space indent and some slack. Scale 1 means plain text.
queryScale :: Int -> Text -> Int
queryScale width query = max 1 (min 4 ((width - 4) `div` max 1 (T.length query)))
