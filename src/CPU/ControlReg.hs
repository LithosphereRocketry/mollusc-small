module CPU.ControlReg where

import Clash.Prelude

controlRegs :: Signal dom (Unsigned 32, Maybe (Unsigned 32)) -> Signal dom (Unsigned 32)
controlRegs _ = pure 0
