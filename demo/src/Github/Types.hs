{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Github.Types where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Semigroup   ((<>))
import           Data.Text        (Text)
import           Servant.API

type Owner = String
type Repo = String
type PullRequestNumber = Int

type UserAgent = String

data PullRequestState = Open | Merged | Closed deriving (Show, Eq)

instance ToHttpApiData PullRequestState where
  toQueryParam Open   = "open"
  toQueryParam Merged = "closed"
  toQueryParam Closed = "closed"

instance FromHttpApiData PullRequestState where
  parseQueryParam "open"   = Right Open
  parseQueryParam "closed" = Right Closed
  parseQueryParam bad      = Left ("Bad PullRequestState " <> bad)

instance FromJSON PullRequestState where
  parseJSON (Object o) = do
    interpret <$> o .:? "merged" .!= False
              <*> o .: "state"
  parseJSON invalid = typeMismatch "PullRequestState" invalid

instance ToJSON PullRequestState where
  toJSON Open   = String "open"
  toJSON Merged = String "closed"
  toJSON Closed = String "closed"

interpret :: Bool -> Text -> PullRequestState
interpret True  "closed" = Merged
interpret False "closed" = Closed
interpret _     _        = Open

data PullRequest =
  PullRequest { number :: PullRequestNumber
              , title  :: Text
              , state  :: PullRequestState
              , author :: User
              } deriving Show

newtype User = User { login :: Text } deriving Show

instance FromJSON User where
  parseJSON (Object o) = User <$> o .: "login"
  parseJSON invalid    = typeMismatch "User" invalid

instance ToJSON User where
  toJSON u = object [ "login" .= login u ]

instance FromJSON PullRequest where
  parseJSON (Object o) =
    PullRequest <$> o .: "number"
                <*> o .: "title"
                <*> parseJSON (Object o)
                <*> o .: "user"
  parseJSON invalid = typeMismatch "PullRequest" invalid

instance ToJSON PullRequest where
  toJSON PullRequest{..} | state == Merged = object [ "number" .= number
                                                    , "title"  .= title
                                                    , "user"   .= author
                                                    , "state"  .= state
                                                    , "merged" .= True
                                                    ]
                         | otherwise       = object [ "number" .= number
                                                    , "title"  .= title
                                                    , "user"   .= author
                                                    , "state"  .= state
                                                    , "merged" .= False
                                                    ]
