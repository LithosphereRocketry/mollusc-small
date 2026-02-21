module CPU.StateMachine where

import Clash.Prelude
import CPU.Decoder
import CPU.ISA
import HDL.Common

data ExeState = StateDecode | StateLui | StateLur | StateJ | StateA | StateB | StateM | StateAdvance
    deriving (Eq, Show, Generic, NFDataX)

traceName :: ExeState -> Unsigned 64
traceName StateAdvance = embedLabel8 "advance"
traceName StateDecode = embedLabel8 "decode"
traceName StateA = embedLabel8 "load_a"
traceName StateB = embedLabel8 "load_b"
traceName StateM = embedLabel8 "memory"
traceName StateLui = embedLabel8 "lui"
traceName StateLur = embedLabel8 "lur"
traceName StateJ = embedLabel8 "jump"

nextState :: ExeState -> DecodeResult -> Bool -> ExeState

nextState StateDecode _ False = StateAdvance
nextState StateDecode DecodeResult { itype = InstrTypeLong InstrLui } _ = StateLui
nextState StateDecode DecodeResult { itype = InstrTypeLong InstrLur } _ = StateLur
nextState StateDecode DecodeResult { itype = InstrTypeLong InstrJ } _ = StateJ
nextState StateDecode _ _ = StateA

nextState StateAdvance _ _ = StateDecode

nextState StateLui _ _ = StateDecode

nextState StateLur _ _ = StateDecode

nextState StateJ _ _ = StateAdvance

nextState StateA DecodeResult { isrcb = BReg _ } _ = StateB
nextState StateA DecodeResult { itype = InstrTypeJx _ } _ = StateAdvance
nextState StateA DecodeResult { itype = InstrTypeLoad _ } _ = StateM
nextState StateA DecodeResult { itype = InstrTypeStore _ } _ = StateM
nextState StateA _ _ = StateDecode

nextState StateB DecodeResult { itype = InstrTypeJx _ } _ = StateAdvance
nextState StateB DecodeResult { itype = InstrTypeLoad _ } _ = StateM
nextState StateB DecodeResult { itype = InstrTypeStore _ } _ = StateM
nextState StateB _ _ = StateDecode

nextState StateM _ _ = StateDecode

data AMux = ASrcPC | ASrc0 | ASrcReg
    deriving (Eq, Show)
data BMux = BSrcImm (Unsigned 32) | BSrcReg
    deriving (Eq, Show)
data WBMux = WBSrcRes | WBSrcMem | WBSrcCR | WBSrcQ
    deriving (Eq, Show)
data QMux = QSrcRes | QSrcMem

-- Entering state
writeA :: ExeState -> Maybe AMux
writeA StateDecode = Just ASrcPC
writeA StateLui = Just ASrc0
writeA StateA = Just ASrcReg
writeA StateM = Just ASrcReg
writeA _ = Nothing

-- Leaving state, entering state, decode result
writeB :: ExeState -> ExeState -> DecodeResult -> Maybe BMux
writeB _ StateDecode _ = Just (BSrcImm 4)
writeB StateDecode _ DecodeResult { isrcb = Imm x }
    = Just (BSrcImm x)
writeB StateDecode _ DecodeResult { isrcb = BReg _ }
    = Just (BSrcImm (deepErrorX "Immediate value of register operation"))
writeB _ StateB DecodeResult { isrcb = BReg _ } = Just BSrcReg
writeB _ _ _ = Nothing

-- Entering state (no mux, always coming from alu result)
writeQ :: ExeState -> Maybe QMux
writeQ StateLui = Just QSrcRes
writeQ StateLur = Just QSrcRes
writeQ StateJ = Just QSrcRes
writeQ StateA = Just QSrcRes
writeQ StateM = Just QSrcMem
writeQ _ = Nothing

-- Entering state
writeIR :: ExeState -> Bool
writeIR StateDecode = True
writeIR _ = False

writePC :: ExeState -> Bool
writePC StateAdvance = True
writePC StateLui = True
writePC StateLur = True
writePC StateA = True
writePC _ = False

-- Leaving state, entering state, decode
writeReg :: ExeState -> ExeState -> DecodeResult -> Maybe WBMux
writeReg StateAdvance StateDecode _ = Nothing
writeReg StateDecode StateAdvance _ = Nothing
writeReg _ StateAdvance _ = Just WBSrcQ
writeReg StateM StateDecode DecodeResult { itype = InstrTypeStore _ } = Nothing
writeReg StateM StateDecode DecodeResult { itype = InstrTypeLoad AccessControl } = Just WBSrcCR
writeReg StateM StateDecode DecodeResult { itype = InstrTypeLoad _ } = Just WBSrcMem
writeReg _ StateDecode DecodeResult { idest = DPred _ } = Nothing
writeReg _ StateDecode _ = Just WBSrcRes
writeReg _ _ _ = Nothing

-- Entering state, decode
readReg :: ExeState -> DecodeResult -> Unsigned 4
readReg StateA DecodeResult { isrca = AReg x } = x
readReg StateA _ = deepErrorX "Register A of instruction with no reg A"
readReg StateB DecodeResult { isrcb = BReg x } = x
readReg StateB _ = deepErrorX "Register B of instruction with no reg B"
readReg StateM DecodeResult { isrcm = MReg x } = x
readReg StateM _ = deepErrorX "Register M of instruction with no reg M"
readReg _ _ = deepErrorX "Register address of cycle that doesn't fetch a register"
