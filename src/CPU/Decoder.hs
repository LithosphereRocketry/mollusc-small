module CPU.Decoder where

import CPU.ISA
import Clash.Prelude
import GHC.Stack (HasCallStack)

data ASource = AZero | AReg (Unsigned 4) | PC
    deriving (Eq, Show, Generic, NFDataX)
data BSource = BReg (Unsigned 4) | Imm (Unsigned 32)
    deriving (Eq, Show, Generic, NFDataX)
data MSource = MNone | MReg (Unsigned 4)
    deriving (Eq, Show, Generic, NFDataX)
data Destination = DReg (Unsigned 4) | DPred (Unsigned 3, Bool) | DMem
    deriving (Eq, Show, Generic, NFDataX)

data DecodeResult = DecodeResult {
    ipred :: Unsigned 3,
    ipinv :: Bool,
    itype :: InstrType,
    isrca :: ASource,
    isrcb :: BSource,
    isrcm :: MSource,
    idest :: Destination
} deriving (Generic, NFDataX)

typeDecoder :: Unsigned 32 -> InstrType
typeDecoder $(bitPattern "...._...._01.._...._...._...._...._....") = InstrTypeLong InstrJ
typeDecoder $(bitPattern "...._...._10.._...._...._...._...._....") = InstrTypeLong InstrLui
typeDecoder $(bitPattern "...._...._11.._...._...._...._...._....") = InstrTypeLong InstrLur

typeDecoder $(bitPattern "...._...._0000_0000_...._0..._...._....") = InstrTypeNormal InstrAdd
typeDecoder $(bitPattern "...._...._0000_0001_...._0..._...._....") = InstrTypeNormal InstrSub
typeDecoder $(bitPattern "...._...._0000_0010_...._0..._...._....") = InstrTypeNormal InstrAnd
typeDecoder $(bitPattern "...._...._0000_0011_...._0..._...._....") = InstrTypeNormal InstrOr
typeDecoder $(bitPattern "...._...._0000_0100_...._0..._...._....") = InstrTypeNormal InstrXor
typeDecoder $(bitPattern "...._...._0000_0101_...._0..._...._....") = InstrTypeNormal InstrSl
typeDecoder $(bitPattern "...._...._0000_0110_...._0..._...._....") = InstrTypeNormal InstrSr
typeDecoder $(bitPattern "...._...._0000_0111_...._0..._...._....") = InstrTypeNormal InstrSra

typeDecoder $(bitPattern "...._...._0010_0000_...._0..._...._....") = InstrTypeCompare InstrLtu
typeDecoder $(bitPattern "...._...._0010_0001_...._0..._...._....") = InstrTypeCompare InstrLt
typeDecoder $(bitPattern "...._...._0010_0010_...._0..._...._....") = InstrTypeCompare InstrEq
typeDecoder $(bitPattern "...._...._0010_0011_...._0..._...._....") = InstrTypeCompare InstrBit

typeDecoder $(bitPattern "...._...._0011_0000_...._0..._...._....") = InstrTypeLoad $ AccessPhysical AccessSizeWord
typeDecoder $(bitPattern "...._0000_0011_...._...._1..._...._....") = InstrTypeStore $ AccessPhysical AccessSizeWord

typeDecoder $(bitPattern "...._...._0010_0000_...._1..._...._....") = InstrTypeJx InstrJx
typeDecoder _ = InstrTypeInvalid

decoder :: (HasCallStack)
    => Unsigned 32
    -> DecodeResult
decoder instr = DecodeResult {
        ipred = ipred,
        ipinv = ipinv,
        itype = itype,
        isrca = sourceA,
        isrcb = sourceB,
        isrcm = sourceM,
        idest = dest
    } where
        ipred = unpack $ slice d30 d28 instr
        ipinv = not $ bitToBool $ instr ! (31 :: Integer)
        itype = typeDecoder instr
        sourceA = case itype of
            InstrTypeLong InstrJ -> PC
            InstrTypeLong InstrLui -> AZero
            InstrTypeLong InstrLur -> PC
            _ -> AReg $ unpack (slice d15 d12 instr)
        sourceB = case itype of
            InstrTypeLong InstrJ -> Imm $ unpack $ signExtend (slice d21 d0 instr ++# (0 :: BitVector 2))
            InstrTypeLong InstrLui -> Imm $ unpack $ slice d21 d0 instr ++# (0 :: BitVector 10)
            InstrTypeLong InstrLur -> Imm $ unpack $ slice d21 d0 instr ++# (0 :: BitVector 10)
            _ -> if bitToBool $ instr ! (10 :: Integer)
                then Imm $ unpack $ signExtend (slice d9 d0 instr)
                else BReg $ unpack $ slice d3 d0 instr
        sourceM = case itype of
            InstrTypeStore _ -> MReg $ unpack (slice d19 d16 instr)
            _  -> MNone
        dest = case itype of
            InstrTypeStore _ -> DMem
            _ -> DReg $ unpack (slice d27 d24 instr)
