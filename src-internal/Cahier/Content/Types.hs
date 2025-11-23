module Cahier.Content.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics

-- | Metadata for blog posts
data PostMetadata = PostMetadata
  { title :: Text
  , date :: Day
  , author :: Maybe Text
  , tags :: [Text]
  , description :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON PostMetadata where
  parseJSON = withObject "PostMetadata" $ \v ->
    PostMetadata
      <$> v .: "title"
      <*> v .: "date"
      <*> v .:? "author"
      <*> v .:? "tags" .!= []
      <*> v .:? "description"

-- | Metadata for poetry
data PoemMetadata = PoemMetadata
  { title :: Text
  , date :: Day
  , author :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON PoemMetadata where
  parseJSON = withObject "PoemMetadata" $ \v ->
    PoemMetadata
      <$> v .: "title"
      <*> v .: "date"
      <*> v .: "author"

-- | Complete blog post with metadata and content
data BlogPost = BlogPost
  { metadata :: PostMetadata
  , content :: Text
  }
  deriving (Show, Eq)

-- | Complete poem with metadata and content
data Poem = Poem
  { metadata :: PoemMetadata
  , content :: Text
  }
  deriving (Show, Eq)

-- | Union type for any content
data Content
  = Post BlogPost
  | Poetry Poem
  deriving (Show, Eq)
