{-# LANGUAGE FlexibleInstances #-}

module Main where

import TimeTrackerTypes
import Data.Time
import Data.Char
import Data.Monoid
import Control.Monad
import Control.Monad.Error
import System.Locale
import Control.Monad.Identity



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




-- Make Either ErrorT a an instance of monoid for concatenation.
-- WITH short-circuiting
instance (Monoid a) => Monoid (Either TTError a) where
    mempty = (Right mempty)
    mappend (Left x) _ = (Left x)
    mappend _ (Left x) = (Left x)
    mappend (Right a) (Right b) = (Right (a `mappend` b))

renderDuration :: NominalDiffTime -> String
renderDuration = readSeconds . round

parseISO :: String -> Either TTError UTCTime
parseISO str = do
    format <- formatFromDateString str
    let time = parseTime defaultTimeLocale format str
    case time of
        Nothing -> Left $ OtherError $ "Timeformat '" ++
                    format ++ "'' parsed from date string '" ++ str ++ "' is invalid"
        Just t -> Right t



-- Takes a date string in (partial) ISO format and returns a format string
-- E.g 2014-09-15 16:03:01
formatFromDateString :: String -> Either TTError String
formatFromDateString date =
    let splits = split (trim date) ' '
    in  case splits of
        [day] -> parseDayFormat day
        [day, time] -> mconcat [parseDayFormat day, Right " ", parseTimeFormat time]


parseDayFormat :: String -> Either TTError String
parseDayFormat day = let splits = split (trim day) '-'
               in case splits of
                   [] -> Left $ OtherError "At least a year must be supplied"
                   [y] -> Right "%Y"
                   [y,m] -> Right "%Y-%m"
                   [y,m,d] -> Right "%F"



test :: TrackerMonad ()
test = do
	--tf <- return $ lift . lift $ parseTimeFormat "20:33:31"
	return ()

parseTimeFormat :: String ->
parseTimeFormat time = let splits = split (trim time) ':'
                 in case splits of
                    [h] -> return "%H"
                    [h,m] -> return "%R"
                    [h,m,s] -> return "%T"
                    _ -> throwError $ OtherError $ "Invalid time format " ++ time

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