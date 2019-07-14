module TTrack.TimeUtilsSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Time.LocalTime
import           Data.Time.Calendar
import           Data.Time.Clock
import           Control.Exception (evaluate)
import           TTrack.TimeUtils
import           TTrack.Utils
import           Lens.Micro.Platform

spec :: Spec
spec = do
  describe "TTrack.TimeUtils" $ do
    describe "readSeconds" $ do
      it "reads seconds and prints as hours-minutes-seconds" $ do
        readSeconds m `shouldBe` "1m"
        readSeconds h `shouldBe` "1h"
        readSeconds s `shouldBe` "1s"
        readSeconds (2*h + 34*m + 48*s) `shouldBe` "2h34m48s"
      it "causes an error if supplied with a negative number" $
        evaluate (readSeconds (-1)) `shouldThrow` errorCall "readSeconds does not take negative values"
    describe "parseDurationToSeconds" $
      it "parses a duration string and returns the seconds" $ do
        parseDurationToSeconds "1h" `shouldBe` Just h
        parseDurationToSeconds "1m" `shouldBe` Just m
        parseDurationToSeconds "1s" `shouldBe` Just s
        parseDurationToSeconds "gobblydegook" `shouldBe` Nothing
  describe "TTrack.Utils" $
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
        getTz dateTime `shouldBe` tz
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
      it "uses default date when appropriate (TZ)" $ do
        dateTime <- parseDateTimeWithDefault def "16:01:58+01:30"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "uses default date when appropriate (no TZ)" $ do
        dateTime <- parseDateTimeWithDefault def "16:01:58"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 16 1 58
        getTz dateTime `shouldBe` tz
      it "accepts hh:mm setting seconds to 0 and date to default (TZ)" $ do
        dateTime <- parseDateTimeWithDefault def "16:01+01:30"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 16 1 00
        getTz dateTime `shouldBe` minutesToTimeZone 90
      it "accepts hh:mm setting seconds to 0 and date to default (no TZ)" $ do
        dateTime <- parseDateTimeWithDefault def "16:01"
        getDay dateTime `shouldBe` (2019, 7, 10)
        getTod dateTime `shouldBe` TimeOfDay 16 1 00
        getTz dateTime `shouldBe` tz
      it "fails when no string given" $ expectDateParseFailure def ""
      it "fails when invalid string given" $ do
        expectDateParseFailure def "foo"
        expectDateParseFailure def "06-24"
        expectDateParseFailure def "2006-24"
        expectDateParseFailure def "12"

expectDateParseFailure def input =
  case parseDateTimeWithDefault def input of
    Nothing -> pure ()
    Just dt -> expectationFailure (show dt)

getTz :: ZonedTime -> TimeZone
getTz = view _zonedTimeZone

getTod :: ZonedTime -> TimeOfDay
getTod = view ztTimeOfDay

getDay :: ZonedTime -> (Integer, Int, Int)
getDay = view gregorian

s = 1
m = 60
h = m * 60
