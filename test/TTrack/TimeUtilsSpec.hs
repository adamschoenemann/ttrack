module TTrack.TimeUtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import TTrack.TimeUtils

spec :: Spec
spec = do
	describe "TTrack.TimeUtils" $ do
		describe "readSeconds" $ do
			it "reads seconds and prints as hours-minutes-seconds" $ do
				readSeconds m `shouldBe` "1m"
				readSeconds h `shouldBe` "1h"
				readSeconds s `shouldBe` "1s"
				readSeconds (2*h + 34*m + 48*s) `shouldBe` "2h34m48s"
			it "causes an error if supplied with a negative number" $ do
				evaluate (readSeconds (-1)) `shouldThrow` errorCall "readSeconds does not take negative values"
		describe "parseDurationToSeconds" $ do
			it "parses a duration string and returns the seconds" $ do
				parseDurationToSeconds "1h" `shouldBe` Just h
				parseDurationToSeconds "1m" `shouldBe` Just m
				parseDurationToSeconds "1s" `shouldBe` Just s
				parseDurationToSeconds "gobblydegook" `shouldBe` Nothing

h = m * 60
m = 60
s = 1
--spec :: Spec
--spec = do
--	describe "Prelude.head" $ do
--		it "returns the first elemen of the list" $ do
--			head [23 ..] `shouldBe` (23 :: Int)

--		it "returns the first element of an *arbitrary* list" $
--			property $ \x xs -> head (x:xs) == (x :: Int)

--		it "throws an exception if used with an empty list" $ do
--			evaluate (head []) `shouldThrow` anyException