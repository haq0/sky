module Sky.Api.Podcast
  ( Podcast(..)
  , Language(..)
  , toLang
  ) where

import Data.Text

-- Handled languages sofar, should add more in the future, as well as a parser than turns a string into a Language
data Language
  = EN
  | ES
  | FR
  | DE
  | NONE
  deriving (Show, Eq)

-- temp function to do it, however should improve in the future
toLang :: Text -> Language
toLang f
  | f == pack "en" = EN
  | f == pack "es" = ES
  | f == pack "fr" = FR
  | f == pack "de" = DE
  | otherwise = EN

data Podcast = Podcast
  { title :: Text
  , url :: Text
  , description :: Text
  , link :: Text
  , episodeCount :: Int
  , author :: Text
  , categories :: [Text]
  , language :: Language
  , image :: Text
  } deriving (Show, Eq)
