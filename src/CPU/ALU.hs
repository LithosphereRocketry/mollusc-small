module CPU.ALU where

import Clash.Prelude
import CPU.ISA

data AluOp = OpAdd | OpSub | OpAnd | OpOr | OpXor | OpSl | OpSr | OpSra
    deriving (Generic, NFDataX)
data CompOp = CompLtu | CompLt | CompEq | CompBit
    deriving (Generic, NFDataX)

aluOp :: InstrType -> AluOp
aluOp (InstrTypeNormal InstrAdd) = OpAdd
aluOp (InstrTypeNormal InstrSub) = OpSub
aluOp (InstrTypeNormal InstrAnd) = OpAnd
aluOp (InstrTypeNormal InstrOr) = OpOr
aluOp (InstrTypeNormal InstrXor) = OpXor
aluOp (InstrTypeNormal InstrSl) = OpSl
aluOp (InstrTypeNormal InstrSr) = OpSr
aluOp (InstrTypeNormal InstrSra) = OpSra
aluOp (InstrTypeLong _) = OpAdd
aluOp (InstrTypeLoad _) = OpAdd
aluOp (InstrTypeStore _) = OpAdd
aluOp _ = deepErrorX "ALU operation for instruction with undefined ALU mode"

compOp :: InstrType -> CompOp
compOp (InstrTypeCompare InstrLtu) = CompLtu
compOp (InstrTypeCompare InstrLt) = CompLt
compOp (InstrTypeCompare InstrEq) = CompEq
compOp (InstrTypeCompare InstrBit) = CompBit
compOp _ = deepErrorX "Comparison operation for instruction with undefined comparator mode"

sl :: Unsigned 32 -> Unsigned 32 -> Unsigned 32
sl a b | b < 32 = shiftL a (fromIntegral b)
sl _ _ = deepErrorX "Undefined shift left past 32 bits"

sr :: Unsigned 32 -> Unsigned 32 -> Unsigned 32
sr a b | b < 32 = shiftR a (fromIntegral b)
sr _ _ = deepErrorX "Undefined shift right past 32 bits"

sra :: Unsigned 32 -> Unsigned 32 -> Unsigned 32
sra a b | b < 32 = bitCoerce $ shiftR (bitCoerce a :: Signed 32) (fromIntegral b)
sra _ _ = deepErrorX "Undefined arithemtic shift right past 32 bits"

alu :: AluOp -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
alu OpAdd = (+)
alu OpSub = (-)
alu OpAnd = (.&.)
alu OpOr = (.|.)
alu OpXor = xor
alu OpSl = sl
alu OpSr = sr
alu OpSra = sra

comparator :: CompOp -> Unsigned 32 -> Unsigned 32 -> Bool
comparator CompLtu a b = (<) a b
comparator CompLt a b = (<)
  (bitCoerce a :: Signed 32)
  (bitCoerce b :: Signed 32)
comparator CompEq a b = (==) a b
comparator CompBit a b = bitToBool $ pack a ! b
