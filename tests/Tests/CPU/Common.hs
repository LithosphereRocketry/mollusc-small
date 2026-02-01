module Tests.CPU.Common where

import Test.Tasty
import Test.Tasty.HUnit
import Common.Common
import Clash.Prelude
import Text.Printf
import GHC.IO (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text.IO as Tx
import qualified Control.Exception as Exc

verifyOutput :: [Unsigned 8] -> [(Maybe (Unsigned 8), Bool)] -> Assertion
verifyOutput _ [] = assertFailure "Program did not terminate"
verifyOutput [] ((_, True):_) = return ()
verifyOutput _ ((_, True):_) = assertFailure "Program terminated too early"
verifyOutput reference ((Nothing, _):tl) = verifyOutput reference tl
verifyOutput (ref:reftl) ((Just val, _):tl) = do
    assertEqual "Program output incorrect value" ref val
    verifyOutput reftl tl
verifyOutput [] ((Just _, _):_) = assertFailure "Program produced too much output"


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
        let referenceBytes = List.map fromIntegral
                           $ BS.unpack
                           $ unsafePerformIO
                           $ BS.readFile

                           $ exchangeSuffix "_rom.bin" "_verify.bin" romPath
            outStream = sampleN maxCycles $ soc romPath
        verifyOutput referenceBytes outStream
            -- `Exc.catch`
            -- \(e :: HUnitFailure) -> do assemblyTrace maxCycles soc romPath
            --                            Exc.throw e

assemblyTrace :: (KnownDomain dom)
    => Int
    -> (HiddenClockResetEnable dom
        => String -> Signal dom (Maybe (Unsigned 8), Bool))
    -> String
    -> IO ()
assemblyTrace maxCycles soc romPath = do
    let socOut = exposeClockResetEnable (soc romPath) clockGen resetGen enableGen
    vcd <- dumpVCD (0, maxCycles) socOut []
    case vcd of
        Left msg -> error msg
        Right contents -> Tx.writeFile (exchangeSuffix "_rom.bin" "_trace.vcd" romPath) contents
