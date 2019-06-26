module TTrack.DateParsingSpec where

import           Control.Monad.Except
import           Data.Maybe.Extras (fromJustMsg)
import           Data.Time
import           TTrack.Utils (parseISO)
import           TTrack.Types

import           Test.Hspec

spec :: Spec
spec =
  describe "TTrack.DateParsing" $
    describe "parseISO" $ do
      it "parses in utc" $ do
        dt <- runExceptT $ parseISO utc "2018-08-09"
        dt `shouldBe` (Right $ utcDate 2018 8 9)
      it "parses in other timezones (1)" $ do
        dt <- runExceptT $ parseISO (hoursToTimeZone 2) "2018-08-09 08:00:00"
        dt `shouldBe` (Right $ utcDateTime 2018 8 9 6 0 0)
      it "parses in other timezones (2)" $ do
        dt <- runExceptT $ parseISO (hoursToTimeZone 2) "2018-08-09"
        dt `shouldBe` (Right $ utcDateTime 2018 8 8 22 0 0)

utcDate y m d = UTCTime (fromGregorian y m d) 0
utcDateTime y m d h mi s = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h mi s)
