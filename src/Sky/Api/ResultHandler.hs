{-# LANGUAGE OverloadedStrings #-}

module Sky.Api.ResultHandler
  ( 
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parse)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Control.Monad ((>=>))
import Sky.Api.Podcast (Language(..), Podcast(..), toLang)

parsePodcast :: Object -> Parser Podcast
parsePodcast res =
  Podcast
    <$> res .:? "title" .!= ""
    <*> res .:? "url" .!= ""
    <*> res .:? "description" .!= ""
    <*> res .:? "link" .!= ""
    <*> res .:? "episodeCount" .!= 0
    <*> res .:? "author" .!= ""
    <*> (HMS.keys
           <$> (res .:? "categories" .!= (HMS.empty :: HMS.HashMap T.Text Value)))
    <*> (toLang <$> res .:? "language" .!= "")
    <*> res .:? "image" .!= ""

parsePodcasts :: Value -> Parser [Podcast]
parsePodcasts (Object ob) = ob .: "feeds" >>= mapM (parseJSON >=> parsePodcast)
parsePodcasts _ = fail "Expected an object"
