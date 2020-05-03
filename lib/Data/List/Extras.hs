
module Data.List.Extras (safeHead) where

import Data.List (uncons)

safeHead :: [a] -> Maybe a
safeHead = fmap fst . uncons
