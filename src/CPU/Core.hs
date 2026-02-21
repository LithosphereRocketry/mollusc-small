module CPU.Core where

import HDL.Common
import Clash.Prelude
import CPU.StateMachine
import CPU.ISA
import CPU.Decoder
import CPU.ALU
import CPU.ControlReg
import GHC.Stack (HasCallStack)

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
toWriteback :: Maybe WBMux -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 4 -> Maybe (Unsigned 4, Unsigned 32)
toWriteback Nothing _ _ _ _ _ = Nothing
toWriteback (Just WBSrcRes) res _ _ _ addr = Just (addr, res)
toWriteback (Just WBSrcMem) _ mem _ _ addr = Just (addr, mem)
toWriteback (Just WBSrcCR) _ _ cr _  addr = Just (addr, cr)
toWriteback (Just WBSrcQ) _ _ _ q  addr = Just (addr, q)

-- next state
toWritebackPred :: ExeState -> DecodeResult -> Bool -> Maybe (Unsigned 3, Unsigned 1)
toWritebackPred StateDecode DecodeResult {
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
toMemAddr StateDecode pc _ = pc
toMemAddr StateM _ res = res
toMemAddr _ _ _ = deepErrorX "Memory address of state with no memory access"

-- prev state, next state, instr type
toALUOp :: ExeState -> ExeState -> InstrType -> AluOp
toALUOp _ StateAdvance _ = OpAdd
toALUOp StateDecode _ _ = OpAdd
toALUOp StateJ _ _ = OpAdd
toALUOp _ _ t = aluOp t

-- next state, type, reg_val
toMemOut :: ExeState -> InstrType -> Unsigned 32 -> Maybe (Unsigned 32)
toMemOut StateM (InstrTypeStore (AccessPhysical _)) val = Just val
toMemOut _ _ _ = Nothing

toCRWrite :: ExeState -> InstrType -> Unsigned 32 -> Maybe (Unsigned 32)
toCRWrite StateM (InstrTypeStore AccessControl) val = Just val
toCRWrite _ _ _ = Nothing

mollusc :: (HasCallStack, HiddenClockResetEnable dom, ?doTrace :: Bool)
    => Signal dom (Unsigned 32) -- memory in
    -> Signal dom (
        Unsigned 32, -- memory address
        Maybe (Unsigned 32) -- memory write
    )
mollusc mem_in =
        bundle (mem_addr, mem_out)
    where
        mem_in_t = tr "mem_in" mem_in
        pc = tr "PC" $ regEn 0x8000 (writePC <$> newState) res
        state = register StateAdvance newState

        decodeIncoming = decoder <$> mem_in_t
        decodeStored = regEn (deepErrorX "Type of unfetched instruction")
                (state .==. pure StateDecode) decodeIncoming
        decode = mux (state .==. pure StateDecode) decodeIncoming decodeStored

        !state_t = tr "state" $ traceName <$> state
        newState = nextState <$> state <*> decode <*> pred_val
        !newState_t = tr "newState" $ traceName <$> newState

        pred_write = tr "pred_wr" (toWritebackPred <$> newState <*> decode <*> pred_res)
        pred_val = tr "predicate" $ xor
            <$> (1 .==. predicatefile (ipred <$> decode) pred_write)
            <*> (ipinv <$> decode)

        read_addr = tr "reg_read_addr" $ readReg <$> newState <*> decode

        a = tr "tmpA" $ regMaybe (deepErrorX "Uninitialized A value")
                ((toWriteA . writeA <$> newState) <*> pc <*> reg_val)
        b = tr "tmpB" $ regMaybe (deepErrorX "Uninitialized B value")
                ((toWriteB <$> (writeB <$> state <*> newState <*> decode)) <*> reg_val)
        q = tr "tmpQ" $ regMaybe (deepErrorX "Uninitialized Q value")
                ((toWriteQ . writeQ <$> newState) <*> res <*> mem_in_t)

        pred_res = tr "pred_result" $ (comparator . compOp . itype <$> decode) <*> a <*> b
        res = tr "result" $ alu <$> (toALUOp <$> state <*> newState <*> (itype <$> decode)) <*> a <*> b
        reg_write = tr "reg_write" $ toWriteback <$> (writeReg <$> state <*> newState <*> decode)
            <*> res
            <*> mem_in
            <*> cr_read
            <*> q
            <*> (toWriteAddr <$> decode)
        reg_val = tr "reg_read_value" $ regfile read_addr reg_write
        mem_addr = tr "mem_addr" $ toMemAddr <$> newState <*> pc <*> res
        mem_out = tr "mem_out" $ toMemOut <$> newState <*> (itype <$> decode) <*> reg_val

        cr_write = toCRWrite <$> newState <*> (itype <$> decode) <*> reg_val
        cr_read = tr "cr_read" $ controlRegs $ bundle (mem_addr, cr_write)
