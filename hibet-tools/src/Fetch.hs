module Fetch
    ( newFetcher
    ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (stripPrefix)
import Network.HTTP.Client
    ( HttpException
    , Manager
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)

-- | A fetcher returns the body or a human-readable error. @file://@ is handled
-- locally so tests and offline runs do not need a manager or the network.
newFetcher :: IO (String -> IO (Either String BS.ByteString))
newFetcher = do
    manager <- newTlsManager
    pure (fetch manager)

fetch :: Manager -> String -> IO (Either String BS.ByteString)
fetch manager url
    | Just path <- stripPrefix "file://" url = do
        result <- try @IOException (BS.readFile path)
        pure $ either (Left . show) Right result
    | otherwise = do
        result <- try @HttpException $ do
            request <- parseRequest url
            response <- httpLbs request manager
            let code = statusCode (responseStatus response)
            pure $
                if code >= 200 && code < 300
                    then Right (BSL.toStrict (responseBody response))
                    else Left ("HTTP " <> show code <> " for " <> url)
        pure $ either (Left . show) id result
