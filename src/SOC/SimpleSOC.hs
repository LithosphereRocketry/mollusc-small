module SOC.SimpleSOC where

import Clash.Prelude

import CPU.Core (mollusc)
import HDL.Common
import Data.Maybe (isJust)

simpleSocReadMux :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
simpleSocReadMux romData ramData addr
    | addr < 0x8000 = ramData
    | addr < 0x10000 = romData
    | otherwise = deepErrorX "Contents of unmapped RAM"

truncateAddr :: Unsigned 32 -> Unsigned 32
truncateAddr a = (a .&. 0x7FFF) `quot` 4

toTtyOut :: Maybe (Unsigned 32) -> Unsigned 32 -> Maybe (Unsigned 8)
toTtyOut (Just v) 0x01000000 = Just (resize v)
toTtyOut _ _ = Nothing

toRamWrite ::Unsigned 32 -> Maybe (Unsigned 32) -> Maybe (Unsigned 32, Unsigned 32)
toRamWrite _ Nothing = Nothing
toRamWrite addr (Just wr) | addr < 0x8000 = Just (truncateAddr addr, wr)
toRamWrite _ _ = Nothing

simpleSoc :: (?doTrace :: Bool, HiddenClockResetEnable dom) => FilePath -> Signal dom (Maybe (Unsigned 8), Bool)
simpleSoc fpath = bundle (ttyOut, terminate) where
    (memAddr, memWr) = unbundle (mollusc memRd)
    muxAddr = register (deepErrorX "Memory read before startup") memAddr
    terminate = tr "terminate" $ isJust <$> memWr .&&. memAddr .==. 0x01001000
    ramRd = tr "ram_rd" $ blockRam1
        NoClearOnReset
        (SNat @32768)
        (deepErrorX "Uninitialized RAM data")
        (truncateAddr <$> memAddr)
        (tr "mem_wr" $ toRamWrite <$> memAddr <*> memWr)
    romRd = tr "rom_rd" $ unpack <$> romFile (SNat @32768) fpath (truncateAddr <$> memAddr)
    ttyOut = tr "tty_out" $ toTtyOut <$> memWr <*> memAddr
    memRd = simpleSocReadMux <$> romRd <*> ramRd <*> muxAddr