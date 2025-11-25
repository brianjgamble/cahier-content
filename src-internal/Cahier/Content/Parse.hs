module Cahier.Content.Parse (parsePoem, parsePost) where

import Cahier.Content.Types
import Commonmark
import Commonmark.Extensions.Attributes
import Commonmark.Extensions.HardLineBreaks
import Data.Bifunctor (first)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Yaml (decodeEither')

-- | Parse a poem file
parsePoem :: Text -> Either String Poem
parsePoem input = do
  (yamlText, markdownText) <- extractFrontmatter input
  meta <- first show $ decodeEither' (TE.encodeUtf8 yamlText)
  return $ MkPoem meta (markdownToHtml markdownText)

-- | Parse a blog post file
parsePost :: Text -> Either String Post
parsePost input = do
  (yamlText, markdownText) <- extractFrontmatter input
  meta <- first show $ decodeEither' (TE.encodeUtf8 yamlText)
  return $ MkPost meta (markdownToHtml markdownText)

-- Extract YAML frontmatter and markdown content
extractFrontmatter :: Text -> Either String (Text, Text)
extractFrontmatter input =
  case T.lines input of
    ("---" : rest) ->
      case break (== "---") rest of
        (yamlLines, "---" : contentLines) ->
          Right (T.unlines yamlLines, T.unlines contentLines)
        _ -> Left "Missing closing --- delimiter for frontmatter"
    _ -> Left "File must start with --- delimiter"

-- Converts markdown text to HTML and returns it as strict text.
markdownToHtml :: Text -> Text
markdownToHtml inp = do
  let customSyntax = hardLineBreaksSpec <> attributesSpec <> defaultSyntaxSpec
      res = runIdentity $ commonmarkWith customSyntax "inline input" inp
  case res of
    Left _ -> T.empty
    Right (html :: Html ()) -> TL.toStrict $ renderHtml html
