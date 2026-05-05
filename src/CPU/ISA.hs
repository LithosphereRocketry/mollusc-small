module CPU.ISA where

import Prelude
import Clash.Prelude
import Clash.Shockwaves.Waveform

data AccessSize = AccessSizeByte
                | AccessSizeHalf
                | AccessSizeWord
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)

data AccessMode = AccessControl
                | AccessTLB
                | AccessPhysical AccessSize
                | AccessVirtual AccessSize
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)

data InstrTypeLong = InstrJ
                   | InstrLui
                   | InstrLur
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)

data InstrTypeNormal = InstrAdd
                     | InstrSub
                     | InstrAnd
                     | InstrOr
                     | InstrXor
                     | InstrSl
                     | InstrSr
                     | InstrSra
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)

data InstrTypeCompare = InstrLtu
                      | InstrLt
                      | InstrEq
                      | InstrBit
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)

data InstrTypeJx = InstrJx
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)


data InstrType = InstrTypeInvalid
               | InstrTypeLong InstrTypeLong
               | InstrTypeNormal InstrTypeNormal
               | InstrTypeCompare InstrTypeCompare
               | InstrTypeLoad AccessMode
               | InstrTypeStore AccessMode
               | InstrTypeJx InstrTypeJx
    deriving (Eq, Show, Generic, NFDataX, BitPack, Waveform)
