module Tests.CPU.Common where

import Test.Tasty
import Test.Tasty.HUnit
import Common.Common
import Clash.Prelude
import Text.Printf

assemblyTest :: (KnownDomain dom)
    => Int
    -> String
    -> (HiddenClockResetEnable dom
        => String -> Signal dom (Maybe (Unsigned 8), Bool))
    -> String
    -> TestTree
assemblyTest maxCycles socName soc romPath = testCase
    (printf "%s soc=%s max_cycles=%d" romPath socName maxCycles)
    $ do
        let targetOut = readFile $ exchangeSuffix "_rom.bin" "_verify.bin" romPath
            outStream = sampleN maxCycles $ soc romPath
        0 @?= 0
