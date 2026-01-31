module CPU.Core where

import Clash.Prelude
import CPU.StateMachine
import CPU.ISA
import CPU.Decoder
import CPU.ALU

withZeroReg :: (Enum addr, HiddenClockResetEnable dom, NFDataX addr, Num addr, Eq addr, NFDataX a, Num a)
    => (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
    -> (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
withZeroReg backingMemory n addr write = mux (addr .==. pure 0)
    (pure 0)
    (backingMemory n addr write)

regfile :: (HiddenClockResetEnable dom)
    => Signal dom (Unsigned 4)
    -> Signal dom (Maybe (Unsigned 4, Unsigned 32))
    -> Signal dom (Unsigned 32)
regfile = withZeroReg asyncRam (SNat @16)

predicatefile :: (HiddenClockResetEnable dom)
    => Signal dom (Unsigned 3)
    -> Signal dom (Maybe (Unsigned 3, Unsigned 1))
    -> Signal dom (Unsigned 1)
predicatefile = withZeroReg asyncRam (SNat @8)

-- select, res, mem, q
toWriteback :: Maybe WBMux -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 4 -> Maybe (Unsigned 4, Unsigned 32)
toWriteback Nothing _ _ _ _ = Nothing
toWriteback (Just WBSrcRes) res _ _ addr = Just (addr, res)
toWriteback (Just WBSrcMem) _ mem _ addr = Just (addr, mem)
toWriteback (Just WBSrcQ) _ _ q  addr = Just (addr, q)

-- next state
toWritebackPred :: ExeState -> DecodeResult -> Bool -> Maybe (Unsigned 3, Unsigned 1)
toWritebackPred StateFetch DecodeResult {
        itype = InstrTypeCompare _,
        idest = DPred (addr, inv)
    } res = Just (addr, unpack (boolToBV (inv `xor` res)))
toWritebackPred _ _ _ = Nothing

toWriteAddr :: DecodeResult -> Unsigned 4
toWriteAddr DecodeResult { idest = DReg x } = x
toWriteAddr _ = deepErrorX "Destination register of instruction that doesn't write register"

toWriteQ :: Maybe QMux -> Unsigned 32 -> Unsigned 32 -> Maybe (Unsigned 32)
toWriteQ Nothing _ _ = Nothing
toWriteQ (Just QSrcRes) res _ = Just res
toWriteQ (Just QSrcMem) _ mem = Just mem

toWriteA :: Maybe AMux -> Unsigned 32 -> Unsigned 32 -> Maybe (Unsigned 32)
toWriteA Nothing _ _ = Nothing
toWriteA (Just ASrcPC) pc _ = Just pc
toWriteA (Just ASrc0) _ _ = Just 0
toWriteA (Just ASrcReg) _ reg = Just reg

toWriteB :: Maybe BMux -> Unsigned 32 -> Maybe (Unsigned 32)
toWriteB Nothing _  = Nothing
toWriteB (Just BSrcReg) reg = Just reg
toWriteB (Just (BSrcImm imm)) _ = Just imm

-- next state
toMemAddr :: ExeState -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
toMemAddr StateFetch pc _ = pc
toMemAddr StateM _ res = res
toMemAddr _ _ _ = deepErrorX "Memory address of state with no memory access"

mollusc :: (HiddenClockResetEnable dom)
    => Signal dom (Unsigned 32) -- memory in
    -> Signal dom (
        Unsigned 32, -- memory address
        Maybe (Unsigned 32) -- memory write
    )
mollusc mem_in = bundle (mem_addr, mem_out)
    where
        pc = regEn 0 (writePC <$> newState) res
        ir = regEn 0 (writeIR <$> newState) mem_in
        decode = decoder <$> ir
        state = register StateAdvance newState
        newState = nextState <$> state <*> decode <*> pred_val

        pred_val = xor
            <$> (1 .==. predicatefile (ipred <$> decode) (toWritebackPred <$> newState <*> decode <*> pred_res))
            <*> (ipinv <$> decode)

        read_addr = readReg <$> newState <*> decode

        a = regMaybe (deepErrorX "Uninitialized A value") ((toWriteA . writeA <$> newState) <*> pc <*> reg_val)
        b = regMaybe (deepErrorX "Uninitialized B value") ((toWriteB <$> (writeB <$> newState <*> decode)) <*> reg_val)
        q = regMaybe (deepErrorX "Uninitialized Q value") ((toWriteQ . writeQ <$> newState) <*> res <*> mem_in)

        pred_res = comparator . compOp . itype <$> decode <*> a <*> b
        res = alu . aluOp . itype <$> decode <*> a <*> b
        reg_val = regfile read_addr (toWriteback <$>
            (writeReg <$> state <*> newState <*> decode)
            <*> res
            <*> mem_in
            <*> q
            <*> (toWriteAddr <$> decode))
        mem_addr = toMemAddr <$> newState <*> pc <*> res
        mem_out = mux (newState .==. pure StateM)
            (Just <$> reg_val)
            (pure Nothing)

