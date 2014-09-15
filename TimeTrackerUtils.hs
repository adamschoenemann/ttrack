module TimeTrackerUtils where

import Data.Time
import Data.Char
import System.Time.Utils



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

renderDuration :: NominalDiffTime -> String
renderDuration = renderSecs . round

-- Takes a date string in (partial) ISO format and returns a format string
-- E.g 2014-09-15 16:03:01
formatFromDateString :: String -> String
formatFromDateString date = let splits = split (trim date) ' '
							in  case splits of
								[day] -> parseDay day
								[day, time] -> parseDay day ++ " " ++ parseTime time
    where 	parseDay day = 	let splits = split (trim day) '-'
    					   	in case splits of
    					   		[] -> error "At least a year must be supplied"
    					   		[y] -> "%Y"
    					   		[y,m] -> "%Y-%m"
    					   		[y,m,d] -> "%F"
    		parseTime time = let splits = split (trim time) ':'
    						 in case splits of
    						 	[] -> ""
    						 	[h] -> "%H"
    						 	[h,m] -> "%R"
    						 	[h,m,s] -> "%T"
