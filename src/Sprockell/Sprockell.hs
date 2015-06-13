{-# LANGUAGE RecordWildCards #-}

module Sprockell.Sprockell where

import Data.Bits
import Data.Maybe
import Debug.Trace
import Sprockell.Components
import Sprockell.TypesEtc

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| Initial definition: October 2012, Jan Kuper (j.kuper@utwente.nl)
| Extensions: June 2015, Martijn Bastiaan, Arjan Boeijink, Jan Kuper, Leon Schoorl
-------------------------------------------------------------}


{-------------------------------------------------------------
| some constants
-------------------------------------------------------------}

initSprockell :: Int -> Value -> SprockellState
initSprockell dataMemSize ident = SprState 
  { regbank  = initRegFile 0 <<~~ [(SPID, ident), (SP, fromIntegral dataMemSize)]
  , localMem = initMemory
  , halted   = False
  }

nullcode :: MachCode
nullcode = MachCode
        { ldCode      = LdImm
        , stCode      = StNone
        , aguCode     = AguImm
        , aluCode     = Or
        , condCode    = CFalse
        , target      = TRel
        , ioCode      = IONone
        , immValue    = 0
        , inputX      = Zero
        , inputY      = Zero
        , result      = Zero
        , loadReg     = Zero
        , addrImm     = 0
        , deref       = Zero
        }

{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}
sprockell :: InstructionMem -> SprockellState -> Maybe Reply -> (SprockellState, Maybe Request)
sprockell instrs SprState{..} reply = (sprState, request)
    where
        pc           = regbank!PC
        MachCode{..} = decode (instrs!pc)

        regX         = regbank!inputX
        regY         = regbank!inputY
        aluOutput    = alu aluCode regX regY

        regAddr      = regbank!deref
        address      = agu aguCode addrImm regAddr

        loadValue    = loadUnit localMem ldCode address reply immValue
        localMem'    = storeUnit localMem stCode address regY

        request      = sendOut ioCode address regY
        
        cond         = condition condCode regX (isJust reply)
        jumpTarget   = targetPC target pc regY immValue
        nextPC       = if cond then jumpTarget else pc + 1

        regbank'     = regbank <<~~ [(result, aluOutput), (loadReg, loadValue), (PC, nextPC), (Zero, 0)]

        sHalted      = condCode == CTrue && target == TRel && immValue == 0
        sprState     = SprState {localMem=localMem' ,regbank=regbank', halted=sHalted}


-- ============================
decode :: Instruction -> MachCode
decode instr = case instr of
    Nop                  -> nullcode
    Compute c rx ry res  -> nullcode {aluCode=c, inputX=rx, inputY=ry, result=res}
    Const n r            -> nullcode {ldCode=LdImm, immValue=n, loadReg=r}
    
    Branch cr (Abs n)    -> nullcode {condCode=CReg, inputX=cr, target=TAbs, immValue=n}
    Branch cr (Rel n)    -> nullcode {condCode=CReg, inputX=cr, target=TRel, immValue=n}
    Branch cr (Ind i)    -> nullcode {condCode=CReg, inputX=cr, target=TInd, inputY=i}
    Jump   (Abs n)       -> nullcode {condCode=CTrue, target=TAbs, immValue=n}
    Jump   (Rel n)       -> nullcode {condCode=CTrue, target=TRel, immValue=n}
    Jump   (Ind i)       -> nullcode {condCode=CTrue, target=TInd, inputY=i}

    Load  (Addr a)  r    -> nullcode {ldCode=LdMem, aguCode=AguImm, addrImm=a, loadReg=r}
    Load  (Deref p) r    -> nullcode {ldCode=LdMem, aguCode=AguDeref, deref=p, loadReg=r}
    Store r (Addr a)     -> nullcode {stCode=StMem, inputY=r, aguCode=AguImm, addrImm=a}
    Store r (Deref p)    -> nullcode {stCode=StMem, inputY=r, aguCode=AguDeref, deref=p}

    Push r               -> nullcode {stCode=StMem, inputY=r, aguCode=AguDown, deref=SP, aluCode=Decr, inputX=SP, result=SP}
    Pop r                -> nullcode {ldCode=LdMem, loadReg=r, aguCode=AguDeref, deref=SP, aluCode=Incr, inputX=SP, result=SP}

    Receive r            -> nullcode {ldCode=LdInp, loadReg=r, condCode=CWait, target=TRel, immValue=0}
    Read (Addr a)        -> nullcode {ioCode=IORead, aguCode=AguImm, addrImm=a}
    Read (Deref p)       -> nullcode {ioCode=IORead, aguCode=AguDeref, deref=p}
    TestAndSet (Addr a)  -> nullcode {ioCode=IOTest, aguCode=AguImm, addrImm=a}
    TestAndSet (Deref p) -> nullcode {ioCode=IOTest, aguCode=AguDeref, deref=p}
    Write r (Addr a)     -> nullcode {ioCode=IOWrite, aguCode=AguImm, addrImm=a, inputY=r}
    Write r (Deref p)    -> nullcode {ioCode=IOWrite, aguCode=AguDeref, deref=p, inputY=r}

    EndProg              -> nullcode {condCode=CTrue, target=TRel, immValue=0}
    Debug _              -> nullcode


-- ============================
alu :: Operator -> Value -> Value -> Value
alu opCode x y = case opCode of
        Incr   -> x + 1
        Decr   -> x - 1
        Add    -> x + y
        Sub    -> x - y
        Mul    -> x * y
        Div    -> x `div` y
        Mod    -> x `mod` y
        Equal  -> intBool (x == y)
        NEq    -> intBool (x /= y)
        Gt     -> intBool (x > y)
        GtE    -> intBool (x >= y)
        Lt     -> intBool (x < y)
        LtE    -> intBool (x <= y)
        And    -> x .&. y
        Or     -> x .|. y
        LShift -> shiftL x (fromIntegral y)
        RShift -> shiftR x (fromIntegral y)
        Xor    -> x `xor` y

intBool :: Bool -> Value
intBool True  = 1
intBool False = 0

-- ============================
agu :: AguCode -> Address -> Value -> Address
agu aguCode addr derefAddr = case aguCode of
        AguImm   -> addr
        AguDeref -> derefAddr
        AguDown  -> derefAddr - 1
        
-- ============================
loadUnit :: LocalMem -> LdCode -> Address -> Maybe Reply -> Value -> Value
loadUnit mem ldCode address reply immval = case (ldCode, reply) of
        (LdImm, Nothing) -> immval
        (LdMem, Nothing) -> mem !!! address
        (LdInp, Just rx) -> rx
        (LdInp, Nothing) -> 0
        (_    , Just rx) -> error ("Sprockell ignored a system response of value: " ++ show rx)

-- ============================
storeUnit :: LocalMem -> StCode -> Address -> Value -> LocalMem
storeUnit mem stCode address value = case stCode of
        StNone -> mem
        StMem  -> mem <~= (address, value)

-- ============================
condition :: CondCode -> Value -> Bool -> Bool
condition cCode cReg hasInput = case cCode of
        CFalse -> False
        CTrue  -> True
        CReg   -> cReg /= 0
        CWait  -> not hasInput

targetPC :: TargetCode -> CodeAddr -> Value -> Value -> CodeAddr
targetPC tCode pc fromind immval = case tCode of
        TAbs -> immval
        TRel -> pc + immval
        TInd -> fromind

-- ============================
sendOut :: IOCode -> Address -> Value -> Maybe Request
sendOut ioCode address value = case ioCode of
        IONone    -> Nothing
        IORead    -> Just (address, ReadReq)
        IOWrite   -> Just (address, WriteReq value)
        IOTest    -> Just (address, TestReq)
