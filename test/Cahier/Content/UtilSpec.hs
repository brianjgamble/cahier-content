module Cahier.Content.UtilSpec (spec) where

import Cahier.Content.Util (slugify)
import Data.Text qualified as T
import Test.Hspec

spec :: Spec
spec = do
  describe "slugify" do
    it "allows letters and numbers" $ do
      slugify (T.pack "ThIs Is A MiXeD CaSe!") `shouldBe` T.pack "this-is-a-mixed-case"

    it "converts letters to lowercase" $ do
      slugify (T.pack "ThIs Is A MiXeD CaSe!") `shouldBe` T.pack "this-is-a-mixed-case"

    it "removes leading and trailing spaces" $ do
      slugify (T.pack "  * Hello, World *  ") `shouldBe` T.pack "hello-world"

    it "removes spaces between words" $ do
      slugify (T.pack "  Hello     World  ") `shouldBe` T.pack "hello-world"

    it "collapses multiple hyphens" $ do
      slugify (T.pack "Hello---World") `shouldBe` T.pack "hello-world"

    it "returns an empty string" $ do
      slugify (T.pack "") `shouldBe` T.pack ""

    it "strips special characters" $ do
      slugify (T.pack "!@#$%^&*()") `shouldBe` T.pack ""

    it "strips leading and trailing hyphens" $ do
      slugify (T.pack "--Hello World--") `shouldBe` T.pack "hello-world"
