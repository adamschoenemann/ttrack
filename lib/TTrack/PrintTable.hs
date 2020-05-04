{-# LANGUAGE ScopedTypeVariables #-}

module TTrack.PrintTable where

import           Data.List (intercalate, intersperse, transpose)
import           Data.Map ((!))
import qualified Data.Map as M

printTable :: (Ord k, Show k) => String -> M.Map k [String] -> String
printTable sep table = intercalate "\n"
  $ intercalate sep headers
  :map (intercalate sep) (transpose columns)
  where
    headers = map (\k -> pad (lengths ! k) $ show k) $ M.keys table

    columns = M.elems $ M.mapWithKey (\k v -> map (pad (lengths ! k)) v) table

    lengths = M.mapWithKey
      (\k v -> maximum (length (show k):map length v))
      table

pad :: Int -> String -> String
pad len s
  | length s < len = s ++ replicate (len - length s) ' '
  | otherwise = s
