module Cahier.Content.Util (slugify) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Text (Text)
import Data.Text qualified as T

{- | Convert a title to a URL-friendly slug.

>>> slugify "Hello, World"
"hello-world"
-}
slugify :: Text -> Text
slugify =
  T.intercalate "-"
    . filter (not . T.null)
    . T.split (== '-')
    . T.map (\c -> if isSpace c || c == '-' then '-' else toLower c)
    . T.filter (\c -> isAlphaNum c || isSpace c || c == '-')
