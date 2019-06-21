module TTrack.DateParsingSpec where

import           Data.Maybe.Extras (fromJustMsg)
import           Data.Time
import           TTrack.Tests (parseISO)

import           TTrack.DateParsing

import           Test.Hspec

spec :: Spec
spec = do
  describe "TTrack.DateParsing" $ do
      describe "parseISO" $ do
        it "parses" $ do
          parseISO "2018-08-09" `shouldBe` Right (utcDate 2018 8 9)

utcDate y m d = UTCTime (fromGregorian y m d) 0

oct22nd2014 = utctDay
  $ fromJustMsg "oct22nd2014"
  $ parseTimeM True defaultTimeLocale "%F" "2014-10-22"
