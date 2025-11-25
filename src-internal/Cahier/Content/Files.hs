module Cahier.Content.Files (loadAllPoems, loadAllPosts) where

import Cahier.Content.Parse
import Cahier.Content.Types
import Cahier.Content.Util (slugify)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Error (catchIOError, isDoesNotExistError)

-- | Load all poems from a directory
loadAllPoems :: FilePath -> IO ([(Text, Poem)], [String])
loadAllPoems dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return ([], [])
    else do
      files <- listDirectory dir
      let mdFiles = filter (\f -> takeExtension f == ".md") files
          paths = map (dir </>) mdFiles

      results <- mapM loadPoemWithSlug paths
      let (errors, poems) = partitionEithers results
      return (poems, errors)
 where
  loadPoemWithSlug path = do
    result <- loadPoem path
    return $ case result of
      Left err -> Left $ path ++ ": " ++ err
      Right poem@(MkPoem (MkPoemMetadata title _ _) _) ->
        Right (slugify title, poem)

-- | Load all blog posts from a directory
loadAllPosts :: FilePath -> IO ([(Text, Post)], [String])
loadAllPosts dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return ([], [])
    else do
      files <- listDirectory dir
      let mdFiles = filter (\f -> takeExtension f == ".md") files
          paths = map (dir </>) mdFiles

      results <- mapM loadPostWithSlug paths
      let (errors, posts) = partitionEithers results
      return (posts, errors)
 where
  loadPostWithSlug path = do
    result <- loadPost path
    return $ case result of
      Left err -> Left $ path ++ ": " ++ err
      Right post@(MkPost (MkPostMetadata title _ _ _ _) _) ->
        Right (slugify title, post)

-- Load a poem from a file
loadPoem :: FilePath -> IO (Either String Poem)
loadPoem path = do
  result <-
    catchIOError
      (Right <$> TIO.readFile path)
      ( \e ->
          if isDoesNotExistError e
            then return $ Left $ "File not found: " ++ path
            else return $ Left $ show e
      )
  case result of
    Left err -> return $ Left err
    Right content -> return $ parsePoem content

-- Load a blog post from a file
loadPost :: FilePath -> IO (Either String Post)
loadPost path = do
  result <-
    catchIOError
      (Right <$> TIO.readFile path)
      ( \e ->
          if isDoesNotExistError e
            then return $ Left $ "File not found: " ++ path
            else return $ Left $ show e
      )
  case result of
    Left err -> return $ Left err
    Right content -> return $ parsePost content

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
 where
  left a (l, r) = (a : l, r)
  right b (l, r) = (l, b : r)
