{-# LANGUAGE RecordWildCards #-}

module Sprockell where

import Data.Bits
import Data.Maybe
import Debug.Trace
import TypesEtc

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| j.kuper@utwente.nl
| October 28, 2012
-------------------------------------------------------------}


{-------------------------------------------------------------
| some constants
-------------------------------------------------------------}

dmemsize    = 128 :: Int    -- TODO: memory sizes as yet unused, no "out of memory" yet

initSprockell :: Int -> SprockellState
initSprockell ident = SprState {regbank = regvalues, localMem = replicate dmemsize 0, halted=False}
    where
        regbanksize =  fromEnum (maxBound::Reg) - fromEnum (minBound::Reg)
        regvalues = (replicate regbanksize 0) <<~ [ (fromEnum SPID, ident)
                                                  , (fromEnum SP, dmemsize - 1) ]

nullcode :: MachCode
nullcode = MachCode
        { ldCode      = LdImm
        , stCode      = StNone
        , aguCode     = AguImm
        , aluCode     = Or
        , pcCode      = PCNext
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

-- ============================
decode :: Instruction -> MachCode
decode instr  = case instr of
    Nop                      -> nullcode
    Compute c rx ry res      -> nullcode {aluCode=c, inputX=rx, inputY=ry, result=res}
    Const n r                -> nullcode {ldCode=LdImm, immValue=n, loadReg=r}
    
    Branch cr (Abs n)        -> nullcode {pcCode=PCBranch TAbs, inputX=cr, immValue=n}
    Branch cr (Rel n)        -> nullcode {pcCode=PCBranch TRel, inputX=cr, immValue=n}
    Branch cr (Ind i)        -> nullcode {pcCode=PCBranch TInd, inputX=cr, inputY=i   }
    Jump   (Abs n)           -> nullcode {pcCode=PCJump TAbs, immValue=n}
    Jump   (Rel n)           -> nullcode {pcCode=PCJump TRel, immValue=n}
    Jump   (Ind i)           -> nullcode {pcCode=PCJump TInd, inputY=i}

    Load  (Addr a)  r        -> nullcode {ldCode=LdMem, aguCode=AguImm, addrImm=a, loadReg=r}
    Load  (Deref p) r        -> nullcode {ldCode=LdMem, aguCode=AguDeref, deref=p, loadReg=r}
    Store r (Addr a)         -> nullcode {stCode=StMem, inputY=r, aguCode=AguImm, addrImm=a}
    Store r (Deref p)        -> nullcode {stCode=StMem, inputY=r, aguCode=AguDeref, deref=p}

    Push r                   -> nullcode {stCode=StMem, inputY=r, aguCode=AguDown, deref=SP, aluCode=Decr, inputX=SP, result=SP}
    Pop r                    -> nullcode {ldCode=LdMem, loadReg=r, aguCode=AguDeref, deref=SP, aluCode=Incr, inputX=SP, result=SP}

    Receive r                -> nullcode {ldCode=LdInp, pcCode=PCWait, loadReg=r}
    Read (Addr a)            -> nullcode {ioCode=IORead, aguCode=AguImm, addrImm=a}
    Read (Deref p)           -> nullcode {ioCode=IORead, aguCode=AguDeref, deref=p}
    TestAndSet (Addr a)      -> nullcode {ioCode=IOTest, aguCode=AguImm, addrImm=a}
    TestAndSet (Deref p)     -> nullcode {ioCode=IOTest, aguCode=AguDeref, deref=p}
    Write r (Addr a)         -> nullcode {ioCode=IOWrite, aguCode=AguImm, addrImm=a, inputY=r}
    Write r (Deref p)        -> nullcode {ioCode=IOWrite, aguCode=AguDeref, deref=p, inputY=r}

    EndProg                  -> nullcode {pcCode=PCJump TRel, immValue=0}
    Debug _                  -> nullcode


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
        Equal  -> tobit (x == y)
        NEq    -> tobit (x /= y)
        Gt     -> tobit (x > y)
        GtE    -> tobit (x >= y)
        Lt     -> tobit (x < y)
        LtE    -> tobit (x <= y)
        And    -> x .&. y
        Or     -> x .|. y
        LShift -> shiftL x y
        RShift -> shiftR x y
        Xor    -> x `xor` y

tobit :: Bool -> Value
tobit True  = 1
tobit False = 0

-- ============================
agu :: AguCode -> Address -> Value -> Address
agu aguCode addr derefAddr = case aguCode of
        AguImm   -> addr
        AguDeref -> derefAddr
        AguDown  -> derefAddr - 1
        
-- ============================
load :: Memory -> LdCode -> (Value, Address, Maybe Value) -> Value
load mem ldCode (immval, address, input) = case ldCode of
        LdImm -> immval
        LdMem -> mem !! address
        LdInp -> fromMaybe 0 input

-- ============================
store :: Memory -> StCode -> Address -> Value -> Memory
store mem stCode address value = case stCode of
        StNone -> mem
        StMem  -> mem <~ (address, value)

-- ============================
pcUpd :: PCCode -> CodeAddr -> Value -> Value -> Bool -> Value -> CodeAddr
pcUpd pcCode pc cond fromind hasInput immval = case pcCode of
        PCNext     -> succ pc
        PCJump   t -> target t
        PCBranch t -> if cond /= 0 then target t else succ pc
        PCWait     -> if hasInput then succ pc else pc
    where target t = case t of
                TAbs  -> immval
                TRel  -> pc + immval
                TInd  -> fromind

-- ============================
sendOut :: IOCode -> Address -> Value -> Maybe SprockellOut
sendOut ioCode address value = case ioCode of
        IONone    -> Nothing
        IORead    -> Just $ (address, ReadReq)
        IOWrite   -> Just $ (address, WriteReq value)
        IOTest    -> Just $ (address, TestReq)

-- ======================================================================================
-- Putting it all together
sprockell :: [Instruction] -> SprockellState -> Maybe Value -> (SprockellState, Maybe SprockellOut)
sprockell instrs  SprState{..} inputVal = (sprState, output) where
        pc            = regbank !! fromEnum PC
        MachCode{..}  = decode (instrs !! pc)

        regX          = regbank !! fromEnum inputX
        regY          = regbank !! fromEnum inputY
        aluOutput     = alu aluCode regX regY

        regAddr       = regbank !! fromEnum deref
        address       = agu aguCode addrImm regAddr

        loadValue     = load localMem ldCode (immValue, address, inputVal)
        localMem'     = store localMem stCode address regY

        nextPC        = pcUpd pcCode pc regX regY (isJust inputVal) immValue

        regbank'      = regbank <<~ [ (fromEnum result, aluOutput)
                                    , (fromEnum loadReg, loadValue)
                                    , (fromEnum Zero, 0)
                                    , (fromEnum PC, nextPC) ]

        sHalted       = pcCode == PCJump TRel && immValue == 0
        sprState      = SprState {localMem=localMem' ,regbank=regbank', halted=sHalted}

        -- Managed by System
        output        = sendOut ioCode address regY

-- ==========================================================================================================

xs <~ (i,x) = take i xs ++ [x] ++ drop (i+1) xs         -- TODO: note the effect for i >= length xs
xs <<~ updates = foldl (<~) xs updates
