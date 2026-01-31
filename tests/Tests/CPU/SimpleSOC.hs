module Tests.CPU.SimpleSOC where

import Clash.Prelude
import HDL.Common
import Tests.CPU.Common

import Test.Tasty
import qualified System.FilePath.Glob as Glob
import CPU.Core (mollusc)

simpleSocReadMux :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
simpleSocReadMux romData ramData addr
    | addr < 0x8000 = ramData
    | addr < 0x10000 = romData
    | otherwise = deepErrorX "Contents of unmapped RAM"

simpleSoc :: (HiddenClockResetEnable dom) => FilePath -> Signal dom (Maybe (Unsigned 8), Bool)
simpleSoc fpath = bundle (ttyOut, terminate) where
    (memAddr, memWr) = unbundle (mollusc memRd)
    terminate = pure False
    ramRd = blockRam1
        NoClearOnReset
        (SNat @32768)
        (deepErrorX "Uninitialized RAM data")
        memAddr
        (makeWriteTuple <$> memAddr <*> memWr)
    romRd = unpack <$> romFile (SNat @32768) fpath memAddr
    ttyOut = (resize <$>) <$> (gateMaybe <$> (memAddr .==. pure 0x01000000) <*> memWr)
    memRd = simpleSocReadMux <$> romRd <*> ramRd <*> memAddr

socTests :: TestTree
socTests = testGroup "Simple SOC" [
    ]
    where
        fnames = Glob.glob "test-data/*_rom.bin"
