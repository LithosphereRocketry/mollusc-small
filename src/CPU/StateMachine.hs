module CPU.StateMachine where

import Clash.Prelude
import CPU.Decoder
import CPU.ISA

data ExeState = StateFetch | StateLui | StateLur | StateJ | StateA | StateB | StateM | StateAdvance
    deriving (Eq, Show, Generic, NFDataX)

nextState :: ExeState -> DecodeResult -> Bool -> ExeState

nextState StateFetch _ False = StateAdvance
nextState StateFetch DecodeResult { itype = InstrTypeLong InstrLui } _ = StateLui
nextState StateFetch DecodeResult { itype = InstrTypeLong InstrLur } _ = StateLur
nextState StateFetch DecodeResult { itype = InstrTypeLong InstrJ } _ = StateJ
nextState StateFetch _ _ = StateB

nextState StateAdvance _ _ = StateFetch

nextState StateLui _ _ = StateFetch

nextState StateLur _ _ = StateFetch

nextState StateJ _ _ = StateAdvance

nextState StateA DecodeResult { isrcb = BReg _ } _ = StateB
nextState StateA DecodeResult { itype = InstrTypeJx _ } _ = StateAdvance
nextState StateA DecodeResult { itype = InstrTypeLoad _ } _ = StateM
nextState StateA DecodeResult { itype = InstrTypeStore _ } _ = StateM
nextState StateA _ _ = StateFetch

nextState StateB DecodeResult { itype = InstrTypeJx _ } _ = StateAdvance
nextState StateB DecodeResult { itype = InstrTypeLoad _ } _ = StateM
nextState StateB DecodeResult { itype = InstrTypeStore _ } _ = StateM
nextState StateB _ _ = StateFetch

nextState StateM _ _ = StateFetch

data AMux = ASrcPC | ASrc0 | ASrcReg
    deriving (Eq, Show)
data BMux = BSrcImm (Unsigned 32) | BSrcReg
    deriving (Eq, Show)
data WBMux = WBSrcRes | WBSrcMem | WBSrcQ
    deriving (Eq, Show)
data QMux = QSrcRes | QSrcMem

-- Entering state
writeA :: ExeState -> Maybe AMux
writeA StateFetch = Just ASrcPC
writeA StateLui = Just ASrc0
writeA StateA = Just ASrcReg
writeA StateM = Just ASrcReg
writeA _ = Nothing

-- Entering state, previous decode result
writeB :: ExeState -> DecodeResult -> Maybe BMux
writeB s DecodeResult { isrcb = Imm x } | s `elem` [StateLui, StateLur, StateJ, StateA]
    = Just (BSrcImm x)
writeB s DecodeResult { isrcb = BReg _ } | s `elem` [StateLui, StateLur, StateJ, StateA]
    = Just (BSrcImm (deepErrorX "Immediate value of register operation"))
writeB StateA DecodeResult { isrcb = BReg _ } = Just BSrcReg
writeB _ _ = Nothing

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
writeIR StateFetch = True
writeIR _ = False

writePC :: ExeState -> Bool
writePC StateAdvance = True
writePC StateLui = True
writePC StateLur = True
writePC StateA = True
writePC _ = False

-- Leaving state, entering state, decode
writeReg :: ExeState -> ExeState -> DecodeResult -> Maybe WBMux
writeReg StateAdvance StateFetch _ = Nothing
writeReg StateFetch StateAdvance _ = Nothing
writeReg _ StateAdvance _ = Just WBSrcQ
writeReg StateM StateFetch DecodeResult { itype = InstrTypeStore _ } = Nothing
writeReg _ StateFetch _ = Just WBSrcRes
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
