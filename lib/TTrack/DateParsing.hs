module TTrack.DateParsing where

import           Data.Time
import           System.Locale hiding (defaultTimeLocale)
import           Control.Applicative ((<$>))

parseDateWithContext :: UTCTime -> String -> Maybe Day
parseDateWithContext context fullFormat =
  utctDay <$> parseTimeM True defaultTimeLocale "%F" fullFormat
