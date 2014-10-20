module TTrack.TimeUtils where

import Data.Time
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many)

data TimeUnit = Hours Integer
              | Minutes Integer
              | Seconds Integer
              deriving (Show, Read, Eq)

timeUnitToSeconds :: TimeUnit -> Integer
timeUnitToSeconds (Hours x) = x * 60 * 60
timeUnitToSeconds (Minutes x) = x * 60
timeUnitToSeconds (Seconds x) = x

timeUnitsToSeconds :: (Maybe TimeUnit, Maybe TimeUnit, Maybe TimeUnit) -> Integer
timeUnitsToSeconds (h,m,s) = f h + f m + f s
    where f Nothing  = 0
          f (Just u) = timeUnitToSeconds u


parseHours = parseTimeUnit Hours 'h'
parseMinutes = parseTimeUnit Minutes 'm'
parseSeconds = parseTimeUnit Seconds 's'

parseTimeUnit :: (Integer -> TimeUnit) -> Char -> CharParser () TimeUnit
parseTimeUnit constructor end =
    constructor <$> (\x -> read x :: Integer) <$> (many digit <* char end)

parseDuration :: CharParser () (Maybe TimeUnit, Maybe TimeUnit, Maybe TimeUnit)
parseDuration = (,,) <$> optionMaybe (try parseHours)
                     <*> optionMaybe (try parseMinutes)
                     <*> optionMaybe (try parseSeconds)