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
  , posts :: Map Text BlogPost -- Keyed by slug
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
getPost :: Text -> ContentCache -> Maybe BlogPost
getPost slug cache = Map.lookup slug (posts cache)

-- | Initialize cache with posts and poems
initCache :: [(Text, BlogPost)] -> [(Text, Poem)] -> ContentCache
initCache posts poems =
  ContentCache
    { poems = poemsMap poems
    , posts = postsMap posts
    , poemsByTitle = sortedPoemDigests poems
    , postsByDate = sortedPostDigests posts
    }

-- Extract title from poem
getPoemTitle :: Poem -> Text
getPoemTitle (Poem (PoemMetadata title _ _) _) = title

-- Extract date from post for sorting
getPostDate :: BlogPost -> Day
getPostDate (BlogPost (PostMetadata _ date _ _ _) _) = date

-- Convert the incoming poem tuples to a map of poems
poemsMap :: [(Text, Poem)] -> Map Text Poem
poemsMap = Map.fromList

-- Convert the incoming post tuples to a map of posts
postsMap :: [(Text, BlogPost)] -> Map Text BlogPost
postsMap = Map.fromList

-- Convert the poems into a sorted list of ContentDigests
sortedPoemDigests :: [(Text, Poem)] -> [ContentDigest]
sortedPoemDigests = map (\(slug, Poem (PoemMetadata title _ _) _) -> ContentDigest title slug) . sortedPoems

-- Convert the posts into a sorted list of ContentDigests
sortedPostDigests :: [(Text, BlogPost)] -> [ContentDigest]
sortedPostDigests = map (\(slug, BlogPost (PostMetadata title _ _ _ _) _) -> ContentDigest title slug) . sortedPosts

-- Sort poems by title
sortedPoems :: [(Text, Poem)] -> [(Text, Poem)]
sortedPoems = sortOn (getPoemTitle . snd)

-- Sort posts by date
sortedPosts :: [(Text, BlogPost)] -> [(Text, BlogPost)]
sortedPosts = sortOn (Down . getPostDate . snd)
