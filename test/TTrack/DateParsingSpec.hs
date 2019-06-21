module TTrack.DateParsingSpec where

import           Control.Monad.Except
import           Data.Maybe.Extras (fromJustMsg)
import           Data.Time
import           TTrack.Utils (parseISO)
import           TTrack.Types

import           Test.Hspec

spec :: Spec
spec = do
  describe "TTrack.DateParsing" $ do
    describe "parseISO" $ do
      it "parses" $ do
        dt <- runExceptT $ parseISO "2018-08-09"
        dt `shouldBe` (Right $ utcDate 2018 8 9)

utcDate y m d = UTCTime (fromGregorian y m d) 0

oct22nd2014 = utctDay
  $ fromJustMsg "oct22nd2014"
  $ parseTimeM True defaultTimeLocale "%F" "2014-10-22"
