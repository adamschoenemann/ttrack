{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TTrack.PrintTableSpec where

import qualified Data.Map as M
import           Data.String (IsString)

import           TTrack.PrintTable

import           Test.Hspec

newtype LitString = LitString { unLitString :: String }
  deriving (Ord, Eq, IsString)

instance Show LitString where
  show = unLitString

data NameAndAge
  = Name
  | Age
  deriving (Eq, Ord, Show)

spec :: Spec
spec = do
  describe "pad"
    $ do
      it "pads correctly with positive lengths"
        $ do
          pad 5 "a" `shouldBe` "a    "
          pad 10 "padme" `shouldBe` "padme     "
      it "doesn't pad with non-positive length"
        $ do
          pad 0 "a" `shouldBe` "a"
          pad (-1) "a" `shouldBe` "a"
      it "doesn't pad if string is longer than padding"
        $ pad 3 "three" `shouldBe` "three"
  describe "printTable"
    $ do
      it "prints the empty map as empty string"
        $ printTable " | " (mempty :: M.Map String [String])
        `shouldBe` (unlines [])
      it "prints the singleton map of 1 row as 1 padded header and 1 row"
        $ printTable " | " (M.singleton ("foo" :: LitString) ["barbar"])
        `shouldBe` "foo   \nbarbar"
      it "counts the headers for padding"
        $ printTable " | " (M.singleton ("barbar" :: LitString) ["foo"])
        `shouldBe` "barbar\nfoo   "
      it "separates the columns with the separator"
        $ printTable
          " | "
          (M.fromList [(Name, ["Johnny"]), (Age, ["9"])])
        `shouldBe` "Name   | Age\nJohnny | 9  "
      it "prints multiple rows correctly"
        $ printTable
          " | "
          (M.fromList [(Name, ["Jack", "Johnny"]), (Age, ["42", "9"])])
        `shouldBe` "Name   | Age\nJack   | 42 \nJohnny | 9  "
