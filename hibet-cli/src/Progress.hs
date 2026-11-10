-- | A one-line progress indicator on @stderr@ for long data operations.
-- With a terminal it redraws one animated line; otherwise it prints one line
-- per step, which keeps logs and test output readable.
module Progress
    ( Progress
    , beginStep
    , endStep
    , updateBytes
    , withProgress
    ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import System.IO (hFlush, hIsTerminalDevice, stderr)

data Progress = Progress
    { terminal :: Bool
    , animate :: Bool
    , label :: IORef Text
    , bytes :: IORef (Maybe (Int64, Maybe Int64))
    , frame :: IORef Int
    , lineOpen :: IORef Bool
    , drawnAt :: IORef Word64
    , lock :: MVar ()
    }

-- | Create a progress line around an action and close it on any exit.
withProgress :: (Progress -> IO a) -> IO a
withProgress action = do
    isTerminal <- hIsTerminalDevice stderr
    animated <- if isTerminal then spinnerEnabled else pure False
    p <-
        Progress
            isTerminal
            animated
            <$> newIORef ""
            <*> newIORef Nothing
            <*> newIORef 0
            <*> newIORef False
            <*> newIORef 0
            <*> newMVar ()
    tid <-
        if animated
            then Just <$> forkIO (forever (threadDelay 100000 >> redraw p))
            else pure Nothing
    action p `finally` stop p tid

beginStep :: Progress -> Text -> IO ()
beginStep p name = withMVar p.lock $ \_ -> do
    writeIORef p.label name
    writeIORef p.bytes Nothing
    writeIORef p.lineOpen True
    when p.terminal (render p)

updateBytes :: Progress -> Int64 -> Maybe Int64 -> IO ()
updateBytes p done total = withMVar p.lock $ \_ -> do
    writeIORef p.bytes (Just (done, total))
    when p.terminal $ do
        now <- getMonotonicTimeNSec
        previous <- readIORef p.drawnAt
        when (now - previous >= 100000000) (render p)

endStep :: Progress -> IO ()
endStep p = withMVar p.lock $ \_ -> do
    closeStep p
    hFlush stderr

redraw :: Progress -> IO ()
redraw p = withMVar p.lock $ \_ -> do
    open <- readIORef p.lineOpen
    when open (render p)

-- The caller holds the lock.
render :: Progress -> IO ()
render p = do
    name <- readIORef p.label
    current <- readIORef p.bytes
    index <- atomicModifyIORef' p.frame (\n -> (n + 1, n))
    let glyph
            | p.animate = T.singleton (spinner !! (index `mod` length spinner)) <> " "
            | otherwise = ""
    TIO.hPutStr stderr ("\r" <> glyph <> name <> bytesSuffix current)
    hFlush stderr
    now <- getMonotonicTimeNSec
    writeIORef p.drawnAt now

stop :: Progress -> Maybe ThreadId -> IO ()
stop p tid = do
    maybe (pure ()) killThread tid
    withMVar p.lock $ \_ -> do
        open <- readIORef p.lineOpen
        when open $ do
            closeStep p
            hFlush stderr

-- The caller holds the lock.
closeStep :: Progress -> IO ()
closeStep p = do
    name <- readIORef p.label
    current <- readIORef p.bytes
    if p.terminal
        then do
            render p
            TIO.hPutStr stderr "\n"
        else TIO.hPutStrLn stderr (name <> bytesSuffix current)
    writeIORef p.lineOpen False

bytesSuffix :: Maybe (Int64, Maybe Int64) -> Text
bytesSuffix Nothing = ""
bytesSuffix (Just (done, Just total))
    | total > 0 =
        " "
            <> megabytes done
            <> "/"
            <> megabytes total
            <> " ("
            <> T.pack (show (done * 100 `div` total))
            <> "%)"
bytesSuffix (Just (done, _)) = " " <> megabytes done

megabytes :: Int64 -> Text
megabytes n =
    T.pack (showFFloat (Just 1) (fromIntegral n / 1000000 :: Double) " MB")

spinner :: String
spinner = "|/-\\"

spinnerEnabled :: IO Bool
spinnerEnabled = do
    noColor <- lookupEnv "NO_COLOR"
    term <- lookupEnv "TERM"
    pure $ maybe True null noColor && term /= Just "dumb"
