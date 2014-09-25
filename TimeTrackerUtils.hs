{-# LANGUAGE FlexibleInstances #-}

module TimeTrackerUtils where

import TimeTrackerTypes
import Data.Time
import Data.Char
import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Monad.Error
import System.Locale


-- Parses a duration of format hms e.g. 1h30m10s
parseDuration :: String -> Maybe NominalDiffTime
parseDuration str = parseDuration' str 0 0
    where
          --                Input     Total time Built-up time
          parseDuration' :: String -> Integer -> Integer -> Maybe NominalDiffTime
          parseDuration' str@(s:rest) t b
                         | isNumber s = parseDuration' (dropNums str) t (readNums str)
                         | isSep s = parseDuration' rest (t + (b * (parseSep s))) 0
                         | isSpace s = parseDuration' rest t b
                         | otherwise = Nothing
          parseDuration' "" t b = Just (fromInteger (t+b) :: NominalDiffTime)
          dropNums str = dropWhile isNumber str
          readNums str = read (takeWhile isNumber str) :: Integer
          isSep c = c `elem` "hms"
          parseSep 's' = 1
          parseSep 'm' = 60
          parseSep 'h' = 60 * 60

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



split :: String -> Char -> [String]
split [] _ = []
split str del = foldr fun [[]] str
    where fun x [[]] = [[x]]
          fun x (y:ys)
                | x == del = []:(trim y):ys
                | otherwise = (x:y):ys


trim :: String -> String
trim = trim' . reverse . trim' . reverse
    where trim' s = dropWhile isSpace s
          isSpace x = x == ' '

-- Make Either ErrorT a an instance of monoid for concatenation.
-- WITH short-circuiting
instance (Monoid a) => Monoid (Either TTError a) where
    mempty = (Right mempty)
    mappend (Left x) _ = (Left x)
    mappend _ (Left x) = (Left x)
    mappend (Right a) (Right b) = (Right (a `mappend` b))

renderDuration :: NominalDiffTime -> String
renderDuration = readSeconds . round

parseISO :: String -> TrackerMonad UTCTime
parseISO str = do
    format <- formatFromDateString str
    let time = parseTime defaultTimeLocale format str
    case time of
        Nothing -> throwError $ OtherError $ "Timeformat '" ++
                    format ++ "'' parsed from date string '" ++ str ++ "' is invalid"
        Just t -> return t



-- Takes a date string in (partial) ISO format and returns a format string
-- E.g 2014-09-15 16:03:01
formatFromDateString :: String -> TrackerMonad String
formatFromDateString date =
    let splits = split (trim date) ' '
    in  case splits of
        [day] -> parseDayFormat day
        [day, time] -> do
            df <- parseDayFormat day
            tf <- parseTimeFormat time
            return $ df ++ " " ++ tf

parseDayFormat :: String -> TrackerMonad String
parseDayFormat day = let splits = split (trim day) '-'
               in case splits of
                   [] -> throwError $ OtherError "At least a year must be supplied"
                   [y] -> return "%Y"
                   [y,m] -> return "%Y-%m"
                   [y,m,d] -> return "%F"

parseTimeFormat :: String -> TrackerMonad String
parseTimeFormat time = let splits = split (trim time) ':'
                 in case splits of
                    [h] -> return "%H"
                    [h,m] -> return "%R"
                    [h,m,s] -> return "%T"
                    _ -> throwError $ OtherError $ "Invalid format " ++ time ++ " was supplied"

parseTimeInput :: String -> TrackerMonad UTCTime
parseTimeInput "now" = liftIO $ getCurrentTime
parseTimeInput "yesterday" = do
    now <- liftIO getCurrentTime
    return $ UTCTime (addDays (-1) $ utctDay now) 0
parseTimeInput "today" = do
    now <- liftIO getCurrentTime
    parseISO $ show $ utctDay now
parseTimeInput i = parseISO i

utcToISO :: UTCTime -> String
utcToISO t = let tc = defaultTimeLocale
             in  formatTime tc "%F %T" t

showSess :: Session -> String
showSess s = (show $ sessStart s) ++ " | " ++ (showEnd s) ++ " | " ++
          (show $ readSeconds $ round $ fromJust $ sessDuration s)
        where showEnd s = case (sessEnd s) of
                            Nothing -> "Unended"
                            Just end -> show end