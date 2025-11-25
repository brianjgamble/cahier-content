module Cahier.Content.TypesSpec (spec) where

import Cahier.Content.Types
import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Test.Hspec

spec :: Spec
spec = parallel do
  describe "PoemMetadata" $ do
    describe "FromJSON instance" $ do
      it "parses valid JSON with all required fields" $ do
        let json =
              object
                [ "title" .= ("Sonnet" :: Text)
                , "date" .= fromGregorian 2024 2 14
                , "author" .= ("Shakespeare" :: Text)
                ]
        let result = fromJSON json :: Result PoemMetadata
        case result of
          Success (MkPoemMetadata title date author) -> do
            title `shouldBe` "Sonnet"
            date `shouldBe` fromGregorian 2024 2 14
            author `shouldBe` "Shakespeare"
          _ -> expectationFailure "Failed to parse"

      it "fails when title is missing" $ do
        let json =
              object
                [ "date" .= fromGregorian 2024 1 1
                , "author" .= ("Poet" :: Text)
                ]
        parseShouldFail (fromJSON json :: Result PoemMetadata)

      it "fails when date is missing" $ do
        let json =
              object
                [ "title" .= ("Poem" :: Text)
                , "author" .= ("Poet" :: Text)
                ]
        parseShouldFail (fromJSON json :: Result PoemMetadata)

      it "fails when author is missing" $ do
        let json =
              object
                [ "title" .= ("Poem" :: Text)
                , "date" .= fromGregorian 2024 1 1
                ]
        parseShouldFail (fromJSON json :: Result PoemMetadata)

    describe "Eq instance" $ do
      it "considers identical metadata equal" $ do
        let meta1 = MkPoemMetadata "Title" (fromGregorian 2024 1 1) "Author"
        let meta2 = MkPoemMetadata "Title" (fromGregorian 2024 1 1) "Author"
        meta1 `shouldBe` meta2

  describe "PostMetadata" $ do
    describe "FromJSON instance" $ do
      it "parses valid JSON with all fields" $ do
        let json =
              object
                [ "title" .= ("Test Post" :: Text)
                , "date" .= fromGregorian 2024 1 15
                , "author" .= ("John Doe" :: Text)
                , "tags" .= (["haskell", "testing"] :: [Text])
                , "description" .= ("A test post" :: Text)
                ]
        let result = fromJSON json :: Result PostMetadata
        case result of
          Success (MkPostMetadata title date author tags desc) -> do
            title `shouldBe` "Test Post"
            date `shouldBe` fromGregorian 2024 1 15
            author `shouldBe` Just "John Doe"
            tags `shouldBe` ["haskell", "testing"]
            desc `shouldBe` Just "A test post"
          _ -> expectationFailure "Failed to parse"

      it "parses JSON with minimal required fields" $ do
        let json =
              object
                [ "title" .= ("Minimal Post" :: Text)
                , "date" .= fromGregorian 2024 1 1
                ]
        let result = fromJSON json :: Result PostMetadata
        case result of
          Success (MkPostMetadata title _ author tags desc) -> do
            title `shouldBe` "Minimal Post"
            author `shouldBe` Nothing
            tags `shouldBe` []
            desc `shouldBe` Nothing
          _ -> expectationFailure "Failed to parse"

      it "fails when title is missing" $ do
        let json = object ["date" .= fromGregorian 2024 1 1]
        parseShouldFail (fromJSON json :: Result PostMetadata)

      it "fails when date is missing" $ do
        let json = object ["title" .= ("No Date" :: Text)]
        parseShouldFail (fromJSON json :: Result PostMetadata)

    describe "Eq instance" $ do
      it "considers identical metadata equal" $ do
        let meta1 = MkPostMetadata "Title" (fromGregorian 2024 1 1) (Just "Author") ["tag"] (Just "Desc")
        let meta2 = MkPostMetadata "Title" (fromGregorian 2024 1 1) (Just "Author") ["tag"] (Just "Desc")
        meta1 `shouldBe` meta2

      it "considers different metadata unequal" $ do
        let meta1 = MkPostMetadata "Title1" (fromGregorian 2024 1 1) Nothing [] Nothing
        let meta2 = MkPostMetadata "Title2" (fromGregorian 2024 1 1) Nothing [] Nothing
        meta1 `shouldNotBe` meta2

  describe "Post" $ do
    it "constructs a post with metadata and content" $ do
      let meta = MkPostMetadata "Title" (fromGregorian 2024 1 1) Nothing [] Nothing
      let MkPost m c = MkPost meta "Content"
      m `shouldBe` meta
      c `shouldBe` "Content"

    it "satisfies Eq instance" $ do
      let meta = MkPostMetadata "Title" (fromGregorian 2024 1 1) Nothing [] Nothing
      let post1 = MkPost meta "Content"
      let post2 = MkPost meta "Content"
      post1 `shouldBe` post2

  describe "Poem" $ do
    it "constructs a poem with metadata and content" $ do
      let meta = MkPoemMetadata "Title" (fromGregorian 2024 1 1) "Author"
      let MkPoem m c = MkPoem meta "Verse"
      m `shouldBe` meta
      c `shouldBe` "Verse"

    it "satisfies Eq instance" $ do
      let meta = MkPoemMetadata "Title" (fromGregorian 2024 1 1) "Author"
      let poem1 = MkPoem meta "Verse"
      let poem2 = MkPoem meta "Verse"
      poem1 `shouldBe` poem2

parseShouldFail :: Result a -> Expectation
parseShouldFail result = case result of
  Error _ -> return ()
  Success _ -> expectationFailure "Expected json parse to fail"
