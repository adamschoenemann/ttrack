{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module TTrack.Utils where

import           TTrack.Types
import           TTrack.TimeUtils
import           Data.Time
import           Data.Char
import           Data.Monoid
import           Data.Foldable (asum)
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Except
import           Lens.Micro.Platform

makeLensesFor
  [("localDay", "_localDay"), ("localTimeOfDay", "_localTimeOfDay")]
  ''LocalTime

makeLensesFor
  [ ("zonedTimeToLocalTime", "_zonedTimeToLocalTime")
  , ("zonedTimeZone", "_zonedTimeZone")
  ]
  ''ZonedTime

makeLensesFor
  [ ("todHour", "_todHour")
  , ("todMin", "_todMin")
  , ("todSec", "_todSec")
  ]
  ''TimeOfDay

_year :: Lens' Day Integer
_year = lens get set where
  get d = let (y, _, _) = toGregorian d in y
  set d y = let (_, m, dom) = toGregorian d in fromGregorian y m dom

_month :: Lens' Day Int
_month = lens get set where
  get d = let (_, m, _) = toGregorian d in m
  set d m = let (y, _, dom) = toGregorian d in fromGregorian y m dom

_dom :: Lens' Day Int
_dom = lens get set where
  get d = let (_, _, dom) = toGregorian d in dom
  set d dom = let (y, m, _) = toGregorian d in fromGregorian y m dom

gregorian :: Lens' ZonedTime (Integer, Int, Int)
gregorian = lens get set where
  get (ZonedTime (LocalTime d _) _) = toGregorian d
  set zt (y, m, dom) = zt & _zonedTimeToLocalTime . _localDay %~ update where
    update d = d & _year .~ y & _month .~ m & _dom .~ dom

ztTimeOfDay :: Lens' ZonedTime TimeOfDay
ztTimeOfDay = lens get set where
  get (ZonedTime (LocalTime _ tod) _) = tod
  set zt tod = zt & _zonedTimeToLocalTime . _localTimeOfDay .~ tod

type MonadTTError m = (MonadError TTError m, MonadIO m)

parseDateTimeWithDefault :: Monad m => ZonedTime -> String -> m ZonedTime
parseDateTimeWithDefault def@(ZonedTime (LocalTime day (TimeOfDay h m s)) tz) input
  | trim input == "" = fail "empty input"
  | otherwise =
    maybe (fail "no format applicable") pure $ asum (map tryfmt attempts)
  where
    tryfmt (format, f) = f <$> parseTimeM True defaultTimeLocale format input
    locale = zonedTimeZone def
    attempts =
      map (_2 %~ (. setTz)) (init attemptsNoTZ)
      <> map (_1 %~ (<> "%Z")) attemptsNoTZ
    attemptsNoTZ :: [(String, ZonedTime -> ZonedTime)]
    attemptsNoTZ =
      [ ("%Y-%m-%dT%H:%M:%S", id)
      , ("%m-%dT%H:%M:%S", setYear)
      , ("%dT%H:%M:%S", setYear . setMonth)
      , ("%H:%M:%S", setYear . setMonth . setDom)
      , ("%M:%S", setYear . setMonth . setDom . setHour)
      , ("%S", setYear . setMonth . setDom . setHour . setMinute)
      , ("", setYear . setMonth . setDom . setHour . setMinute . setSeconds)
      ]
    format = "%Y"
    (year, month, dom) = toGregorian day
    setYear = _zonedTimeToLocalTime . _localDay . _year .~ year
    setMonth = _zonedTimeToLocalTime . _localDay . _month .~ month
    setDom = _zonedTimeToLocalTime . _localDay . _dom .~ dom
    setHour = _zonedTimeToLocalTime . _localTimeOfDay . _todHour .~ h
    setMinute = _zonedTimeToLocalTime . _localTimeOfDay . _todMin .~ m
    setSeconds = _zonedTimeToLocalTime . _localTimeOfDay . _todSec .~ s
    setTz = _zonedTimeZone .~ tz

parseISO :: MonadTTError m => TimeZone -> String -> m UTCTime
parseISO tz str = do
  format <- formatFromDateString str
  let time = parseTimeM True defaultTimeLocale format str
  case time of
    Nothing -> throwError
      $ OtherError
      $ "Timeformat '"
      ++ format
      ++ "'' parsed from date string '"
      ++ str
      ++ "' is invalid"
    Just t  -> pure . zonedTimeToUTC $ t { zonedTimeZone = tz }

-- Takes a date string in (partial) ISO format and returns a format string
-- E.g 2014-09-15 16:03:01
formatFromDateString :: MonadTTError m => String -> m String
formatFromDateString date =
  let splits = split (trim date) ' '
  in case splits of
       [day]       -> parseDayFormat day
       [day, time] -> do
         df <- parseDayFormat day
         tf <- parseTimeFormat time
         return $ df ++ " " ++ tf

parseDayFormat :: MonadTTError m => String -> m String
parseDayFormat day =
  let splits = split (trim day) '-'
  in case splits of
       []        -> throwError $ OtherError "At least a year must be supplied"
       [y]       -> return "%Y"
       [y, m]    -> return "%Y-%m"
       [y, m, d] -> return "%F"

parseTimeFormat :: MonadTTError m => String -> m String
parseTimeFormat time =
  let splits = split (trim time) ':'
  in case splits of
       [h]       -> return "%H"
       [h, m]    -> return "%R"
       [h, m, s] -> return "%T"
       _         -> throwError
         $ OtherError
         $ "Invalid format " ++ time ++ " was supplied"

parseTimeInput :: MonadTTError m => TimeZone -> String -> m UTCTime
parseTimeInput _ "now" = liftIO getCurrentTime
parseTimeInput _ "yesterday" = do
  now <- liftIO getCurrentTime
  return $ UTCTime (addDays (-1) $ utctDay now) 0
parseTimeInput tz "today" = do
  now <- liftIO getCurrentTime
  parseISO tz $ show $ utctDay now
parseTimeInput tz i = parseISO tz i

utcToISO :: UTCTime -> String
utcToISO t = let tc = defaultTimeLocale
             in formatTime tc "%F %T" t

-- guess I did this for fun?
split :: String -> Char -> [String]
split [] _ = []
split str del = foldr fun [[]] str
  where
    fun x [[]] = [[x]]
    fun x (y:ys)
      | x == del = []:(trim y):ys
      | otherwise = (x:y):ys

trim :: String -> String
trim = trim' . reverse . trim' . reverse
  where
    trim' s = dropWhile isSpace s

    isSpace x = x == ' '

renderDuration :: NominalDiffTime -> String
renderDuration = readSeconds . round
