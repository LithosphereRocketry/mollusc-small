module CPU.ISA where

import Prelude

data AccessSize = AccessSizeByte
                | AccessSizeHalf
                | AccessSizeWord
    deriving (Eq, Show)

data AccessMode = AccessControl
                | AccessTLB
                | AccessPhysical AccessSize
                | AccessVirtual AccessSize
    deriving (Eq, Show)

data InstrTypeLong = InstrJ
                   | InstrLui
                   | InstrLur
    deriving (Eq, Show)

data InstrTypeNormal = InstrAdd
                     | InstrSub
                     | InstrAnd
                     | InstrOr
                     | InstrXor
                     | InstrSl
                     | InstrSr
                     | InstrSra
    deriving (Eq, Show)

data InstrTypeCompare = InstrLtu
                      | InstrLt
                      | InstrEq
                      | InstrBit
    deriving (Eq, Show)

data InstrTypeJx = InstrJx
    deriving (Eq, Show)


data InstrType = InstrTypeInvalid
               | InstrTypeLong InstrTypeLong
               | InstrTypeNormal InstrTypeNormal
               | InstrTypeCompare InstrTypeCompare
               | InstrTypeLoad AccessMode
               | InstrTypeStore AccessMode
               | InstrTypeJx InstrTypeJx
    deriving (Eq, Show)
