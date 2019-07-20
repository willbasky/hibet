-- | This module contains functions for colorful printing into terminal.

module Prettify
       ( Color (..)
       , blue
       , blueCode
       , bold
       , boldCode
       , cyan
       , cyanCode
       , endLine
       , green
       , greenCode
       , magenta
       , magentaCode
       , nothingFound
       , putTextFlush
       , redCode
       , red
       , resetCode
       , yellow
       , yellowCode

       -- Unused
       , errorMessage
       , warningMessage
       , successMessage
       , infoMessage
       , skipMessage
       , boldDefault
       , prompt
       , italicIO
       , beautyPrint
       ) where

import           Data.Text (Text)
import           System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                                      ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground),
                                      SGR (..), setSGR, setSGRCode)
import           System.IO (hFlush, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as IO

----------------------------------------------------------------------------
-- Ansi-terminal
----------------------------------------------------------------------------

-- Explicit flush ensures prompt messages are in the correct order on all systems.
putTextFlush :: Text -> IO ()
putTextFlush msg = do
    IO.hPutStrLn stdout msg
    hFlush stdout

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

-- | Starts bold printing.
boldIO :: IO ()
boldIO = setSGR [SetConsoleIntensity BoldIntensity]

italicIO :: IO ()
italicIO = setSGR [SetItalicized True]

-- | Resets all previous settings.
resetIO :: IO ()
resetIO = do
    setSGR [Reset]
    hFlush stdout

-- | Takes list of formatting options, prints text using this format options.
beautyPrint :: [IO ()] -> Text -> IO ()
beautyPrint formats msg = do
    sequence_ formats
    IO.hPutStrLn stdout msg
    resetIO

prompt :: IO Text
prompt = do
    setColor Blue
    putTextFlush "  ->   "
    resetIO
    IO.getLine

boldText :: Text -> IO ()
boldText message = boldIO >> putTextFlush message >> resetIO

boldDefault :: Text -> IO ()
boldDefault message = boldText (" [" <> message <> "]")

colorMessage :: Color -> Text -> IO ()
colorMessage color message = do
    setColor color
    IO.hPutStrLn stdout $ "  " <> message
    resetIO

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage   = colorMessage Red
warningMessage = colorMessage Yellow
successMessage = colorMessage Green
infoMessage    = colorMessage Blue
skipMessage    = colorMessage Cyan

cyanCode, blueCode, boldCode, greenCode, magentaCode, redCode, resetCode, yellowCode :: Text
redCode = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
blueCode = T.pack $ setSGRCode [SetColor Foreground Vivid Blue]
cyanCode = T.pack $ setSGRCode [SetColor Foreground Vivid Cyan]
greenCode = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
yellowCode = T.pack $ setSGRCode [SetColor Foreground Vivid Yellow]
magentaCode = T.pack $ setSGRCode [SetColor Foreground Vivid Magenta]
boldCode = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
resetCode = T.pack $ setSGRCode [Reset]

red, blue, cyan, green, yellow, magenta, bold :: Text -> Text
red t = redCode <> t <> resetCode
blue t = blueCode <> t <> resetCode
cyan t = cyanCode <> t <> resetCode
green t = greenCode <> t <> resetCode
yellow t = yellowCode <> t <> resetCode
magenta t = magentaCode <> t <> resetCode
bold t = boldCode <> t <> resetCode

endLine :: Text
endLine = "\n"

nothingFound :: IO ()
nothingFound = do
    putTextFlush $ red "Nothing found."
    putTextFlush ""
