{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module TTrack.Utils where

import           TTrack.Types
import           TTrack.TimeUtils
import           Data.Time
import           Data.Char
import           Data.Monoid
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Except

type MonadTTError m = (MonadError TTError m, MonadIO m)

parseISO :: MonadTTError m => String -> m UTCTime
parseISO str = do
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
    Just t  -> return t

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

parseTimeInput :: MonadTTError m => String -> m UTCTime
parseTimeInput "now" = liftIO getCurrentTime
parseTimeInput "yesterday" = do
  now <- liftIO getCurrentTime
  return $ UTCTime (addDays (-1) $ utctDay now) 0
parseTimeInput "today" = do
  now <- liftIO getCurrentTime
  parseISO $ show $ utctDay now
parseTimeInput i = parseISO i

utcToISO :: UTCTime -> String
utcToISO t = let tc = defaultTimeLocale
             in formatTime tc "%F %T" t
