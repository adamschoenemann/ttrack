module TTrack.DateParsingSpec where

import Test.Hspec
import Data.Time
import System.Locale
import Data.Maybe (fromJust)
import TTrack.DateParsing
import Control.Monad.Trans

spec :: Spec
spec = do
	describe "TTrack.DateParsing" $ do
		describe "parseDateWithContext" $ do
			it "parses a date and fills out missing information from current date" $ do
				now <- liftIO getCurrentTime
				let today = utctDay now
				let (year,month,day) = toGregorian today
				parseDateWithContext now "2014-10-22" `shouldBe` (Just oct22nd2014)
				parseDateWithContext now "09-22" `shouldBe`
					(Just $ fromGregorian year 09 22)

oct22nd2014 = utctDay $ fromJust $ parseTime defaultTimeLocale "%F" "2014-10-22"
