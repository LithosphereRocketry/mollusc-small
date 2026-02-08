module Tests.CPU.SimpleSOC where

import Clash.Prelude

import Tests.CPU.Common
import SOC.SimpleSOC

import Test.Tasty
import qualified System.FilePath.Glob as Glob
import qualified Data.List as List
import GHC.IO (unsafePerformIO)

socTests :: TestTree
socTests = testGroup "Simple SOC" $ let ?doTrace = False in
    List.map (assemblyTest @System 200000 "small" simpleSoc)
            $ unsafePerformIO $ Glob.glob "test-data/*_rom.bin.txt"
