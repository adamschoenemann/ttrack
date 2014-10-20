module TTrack.TimeUtils
    ( parseDurationToDiffTime
    , readSeconds
    ) where

import Data.Time
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many)

-- parse durations
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

parseDurationToTimeUnits :: CharParser () (Maybe TimeUnit, Maybe TimeUnit, Maybe TimeUnit)
parseDurationToTimeUnits = (,,) <$> optionMaybe (try parseHours)
                                <*> optionMaybe (try parseMinutes)
                                <*> optionMaybe (try parseSeconds)

parseDurationToDiffTime :: String -> Maybe NominalDiffTime
parseDurationToDiffTime s = case (parse parseDurationToTimeUnits "none" s) of
                                    Right x -> Just $ ((fromInteger $ timeUnitsToSeconds x) :: NominalDiffTime)
                                    Left err -> Nothing

-- Display duration from seconds
divRemaind :: Integer -> Integer -> (Integer, Integer)
divRemaind a b = let x = a `div` b
                     r = a - (x*b)
                 in  (x, r)

hours :: Integer -> (Integer, Integer)
hours a = a `divRemaind` (60 * 60)

minutes :: Integer -> (Integer, Integer)
minutes a = a `divRemaind` 60

readSeconds :: Integer -> String
readSeconds s = let (h, s') = hours s
                    (m, s'') = minutes s'
                    format x c = if x > 0 then (show x) ++ c else ""
                in (format h "h") ++ (format m "m") ++ format s'' "s"