module Cahier.Content.Cache (ContentDigest (..), ContentCache, getAllPoems, getAllPosts, getPoem, getPost, initCache) where

import Cahier.Content.Types
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)

-- | Digest information for content listings
data ContentDigest = ContentDigest
  { digestTitle :: Text
  , digestSlug :: Text
  }
  deriving (Show, Eq)

-- | Cache to hold all loaded content
data ContentCache = ContentCache
  { poems :: Map Text Poem -- Keyed by slug
  , posts :: Map Text Post -- Keyed by slug
  , poemsByTitle :: [ContentDigest] -- Sorted by title, ascending
  , postsByDate :: [ContentDigest] -- Sorted by date, newest first
  }
  deriving (Show)

-- | Get all poems sorted by year (newest first)
getAllPoems :: ContentCache -> [ContentDigest]
getAllPoems = poemsByTitle

-- | Get all posts sorted by date (newest first)
getAllPosts :: ContentCache -> [ContentDigest]
getAllPosts = postsByDate

-- | Get a single poem by slug
getPoem :: Text -> ContentCache -> Maybe Poem
getPoem slug cache = Map.lookup slug (poems cache)

-- | Get a single post by slug
getPost :: Text -> ContentCache -> Maybe Post
getPost slug cache = Map.lookup slug (posts cache)

-- | Initialize cache with posts and poems
initCache :: [(Text, Post)] -> [(Text, Poem)] -> ContentCache
initCache posts poems =
  ContentCache
    { poems = poemsMap poems
    , posts = postsMap posts
    , poemsByTitle = sortedPoemDigests poems
    , postsByDate = sortedPostDigests posts
    }

-- Extract title from poem
getPoemTitle :: Poem -> Text
getPoemTitle (MkPoem (MkPoemMetadata title _ _) _) = title

-- Extract date from post for sorting
getPostDate :: Post -> Day
getPostDate (MkPost (MkPostMetadata _ date _ _ _) _) = date

-- Convert the incoming poem tuples to a map of poems
poemsMap :: [(Text, Poem)] -> Map Text Poem
poemsMap = Map.fromList

-- Convert the incoming post tuples to a map of posts
postsMap :: [(Text, Post)] -> Map Text Post
postsMap = Map.fromList

-- Convert the poems into a sorted list of ContentDigests
sortedPoemDigests :: [(Text, Poem)] -> [ContentDigest]
sortedPoemDigests = map (\(slug, MkPoem (MkPoemMetadata title _ _) _) -> ContentDigest title slug) . sortedPoems

-- Convert the posts into a sorted list of ContentDigests
sortedPostDigests :: [(Text, Post)] -> [ContentDigest]
sortedPostDigests = map (\(slug, MkPost (MkPostMetadata title _ _ _ _) _) -> ContentDigest title slug) . sortedPosts

-- Sort poems by title
sortedPoems :: [(Text, Poem)] -> [(Text, Poem)]
sortedPoems = sortOn (getPoemTitle . snd)

-- Sort posts by date
sortedPosts :: [(Text, Post)] -> [(Text, Post)]
sortedPosts = sortOn (Down . getPostDate . snd)
