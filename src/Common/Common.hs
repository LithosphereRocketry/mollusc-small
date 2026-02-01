module Common.Common where

import Prelude
import Data.List (isSuffixOf)

exchangeSuffix :: String -> String -> String -> String
exchangeSuffix from to value = if from `isSuffixOf` value then
  take (length value - length from) value ++ to
  else error "Suffix did not match"