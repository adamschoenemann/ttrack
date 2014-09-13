module Main where

str = "id, name, start, end"

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


test :: [Int] -> [Int]
test xs = foldr (\x acc -> (x+2):acc) [] xs