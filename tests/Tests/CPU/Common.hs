module Tests.CPU.Common where

import Test.Tasty
import Common.Common
import Clash.Prelude

assemblyTest :: FilePath -> Integer -> Bool
assemblyTest _ _ = True

assemblyRun :: FilePath -> Integer -> Maybe [Unsigned 8]
assemblyRun _ _ = Just []
