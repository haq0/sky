{-# LANGUAGE OverloadedStrings #-}

module Sky.Api.Handler
  ( searchPodcasts
  , searchPodcastsByTitle
  , searchPodcastsByPerson
  ) where

import qualified Control.Exception as Control
import Control.Exception (try)
import Crypto.Hash (SHA1(SHA1), hashWith)
import Data.Aeson
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI, mk)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types.Header (Header)

createAuthenticator :: String -> String -> IO [Header]
createAuthenticator apiKey apiSecret = do
  timestamp <- show . round <$> getPOSIXTime
  let authHash =
        convertToBase Base16
          $ hashWith SHA1 (BS.pack $ apiKey ++ apiSecret ++ timestamp)
  return
    [ (mk "X-Auth-Date", BS.pack timestamp)
    , (mk "X-Auth-Key", BS.pack apiKey)
    , (mk "Authorization", authHash)
    ]

searchPodcasts :: String -> String -> String -> IO (Either String Value)
searchPodcasts apiKey apiSecret searchTitle = do
  authHeaders <- createAuthenticator apiKey apiSecret
  let request =
        setRequestMethod "GET"
          $ setRequestHost "api.podcastindex.org"
          $ setRequestPath "/api/1.0/search/byterm"
          $ setRequestQueryString [("q", Just $ BS.pack searchTitle)]
          $ setRequestHeader "User-Agent" ["Sky/1.0"]
          $ setRequestHeaders authHeaders
          $ setRequestSecure True
          $ setRequestPort 443 defaultRequest
  result <- try $ httpJSON request
  case result of
    Left (HttpExceptionRequest req content) ->
      return $ Left $ "HTTP Exception: " ++ show content
    Left (InvalidUrlException url reason) ->
      return $ Left $ "Invalid URL: " ++ url ++ " - " ++ reason
    Right response -> return $ Right $ getResponseBody response

searchPodcastsByTitle :: String -> String -> String -> IO (Either String Value)
searchPodcastsByTitle apiKey apiSecret searchTitle = do
  authHeaders <- createAuthenticator apiKey apiSecret
  let request =
        setRequestMethod "GET"
          $ setRequestHost "api.podcastindex.org"
          $ setRequestPath "/api/1.0/search/bytitle"
          $ setRequestQueryString [("q", Just $ BS.pack searchTitle)]
          $ setRequestHeader "User-Agent" ["Sky/1.0"]
          $ setRequestHeaders authHeaders
          $ setRequestSecure True
          $ setRequestPort 443 defaultRequest
  result <- try $ httpJSON request
  case result of
    Left (HttpExceptionRequest req content) ->
      return $ Left $ "HTTP Exception: " ++ show content
    Left (InvalidUrlException url reason) ->
      return $ Left $ "Invalid URL: " ++ url ++ " - " ++ reason
    Right response -> return $ Right $ getResponseBody response

searchPodcastsByPerson :: String -> String -> String -> IO (Either String Value)
searchPodcastsByPerson apiKey apiSecret searchTitle = do
  authHeaders <- createAuthenticator apiKey apiSecret
  let request =
        setRequestMethod "GET"
          $ setRequestHost "api.podcastindex.org"
          $ setRequestPath "/api/1.0/search/byperson"
          $ setRequestQueryString [("q", Just $ BS.pack searchTitle)]
          $ setRequestHeader "User-Agent" ["Sky/1.0"]
          $ setRequestHeaders authHeaders
          $ setRequestSecure True
          $ setRequestPort 443 defaultRequest
  result <- try $ httpJSON request
  case result of
    Left (HttpExceptionRequest req content) ->
      return $ Left $ "HTTP Exception: " ++ show content
    Left (InvalidUrlException url reason) ->
      return $ Left $ "Invalid URL: " ++ url ++ " - " ++ reason
    Right response -> return $ Right $ getResponseBody response
-- Helper function to wrap tryHttpException (not needed anymore as we're using `try` directly)
-- tryHttpException :: IO a -> IO (Either HttpException a)
-- tryHttpException = try
