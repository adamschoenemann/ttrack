
module Data.Maybe.Extras (fromJustMsg) where

import GHC.Stack (HasCallStack)

fromJustMsg :: HasCallStack => String -> Maybe a -> a
fromJustMsg msg ma =
  case ma of
    Nothing -> error msg
    Just x -> x
