module TTrack.TimeUtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Control.Exception (evaluate)
import TTrack.TimeUtils
import TTrack.Utils
import Lens.Micro.Platform

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
  describe "TTrack.Utils" $ do
    describe "parseDateTimeWithDefault" $ do
      let day = fromGregorian 2019 7 10
      let tz = hoursToTimeZone 2
      let tod = TimeOfDay 9 23 59
      let def = ZonedTime (LocalTime day tod) tz
      it "does not use default if all details given" $ do
        dateTime <- parseDateTimeWithDefault def "2020-06-11T16:01:58+01:30"
        getDay dateTime `shouldBe` (2020, 6, 11)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default timezone when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "2020-06-11T16:01:58"
        getDay dateTime `shouldBe` (2020, 6, 11)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 120
      it "uses default year when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "06-11T16:01:58+01:30"
        getDay dateTime `shouldBe` (2019, 6, 11)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default month when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "11T16:01:58+01:30"
        getDay dateTime `shouldBe` (2019, 7, 11)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default date when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "16:01:58+01:30"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default hour when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "01:58+01:30"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 9 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default minute when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "58+01:30"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 9 23 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default seconds when appropriate" $ do
        dateTime <- parseDateTimeWithDefault def "+01:30"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 9 23 59
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "fails when no string given" $ do
        case parseDateTimeWithDefault def "" of
          Nothing -> pure ()
          Just dt -> expectationFailure (show dt)
        -- parseDateTimeWithDefault def "" `shouldThrow` anyIOException

getTz :: ZonedTime -> TimeZone
getTz = view _zonedTimeZone

getTod :: ZonedTime -> TimeOfDay
getTod = view ztTimeOfDay

getDay :: ZonedTime -> (Integer, Int, Int)
getDay = view gregorian

s = 1
m = 60
h = m * 60
