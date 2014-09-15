module Main where

import System.Exit
import Foreign
import Foreign.C.Types

type Handler = CInt -> IO ()

foreign import ccall "wrapper"
  genHandler:: (Handler) -> IO (FunPtr Handler)

foreign import ccall safe "signal.h signal"
        install:: CInt -> FunPtr Handler -> IO CInt

main = do
        s <- genHandler (\x -> putStrLn $ "catch " ++ (show x))
        res <- install 2 s
        putStrLn $ show res
        s <- getLine
        return ()


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