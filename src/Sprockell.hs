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


initstate ident = SprState {regbank = regvalues, dmem = replicate dmemsize 0, halted=False}
    where
        regvalues = ((replicate regbanksize 0) <~ (fromEnum SPID, ident)) <~ (fromEnum SP, dmemsize - 1)
        regbanksize =  fromEnum (maxBound::Reg) - fromEnum (minBound::Reg)


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
        , sHalted     = False
        }

{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

-- ============================
decode :: Instruction -> MachCode
decode instr  = case instr of
    Nop                      -> nullcode

    Compute c r0 r1 r2       -> nullcode {aluCode=c, inputX=r0, inputY=r1, result=r2}
    
    Branch r0 (Abs n)        -> nullcode {pcCode=PCBranch TAbs, immValue=n, inputX=r0}
    Branch r0 (Rel n)        -> nullcode {pcCode=PCBranch TRel, immValue=n, inputX=r0}
    Branch r0 (Ind i)        -> nullcode {pcCode=PCBranch TInd, deref=i   , inputX=r0}

    Jump   (Abs n)           -> nullcode {pcCode=PCJump TAbs, immValue=n}
    Jump   (Rel n)           -> nullcode {pcCode=PCJump TRel, immValue=n}
    Jump   (Ind r)           -> nullcode {pcCode=PCJump TInd, deref=r}

    Const n r                -> nullcode {ldCode=LdImm, immValue=n, loadReg=r}

    Load  (Addr a)  r        -> nullcode {ldCode=LdMem, aguCode=AguImm, addrImm=a, loadReg=r}
    Load  (Deref p) r        -> nullcode {ldCode=LdMem, aguCode=AguDeref, deref=p, loadReg=r}

    Store r (Addr a)         -> nullcode {stCode=StMem, inputY=r, aguCode=AguImm, addrImm=a}
    Store r (Deref p)        -> nullcode {stCode=StMem, inputY=r, aguCode=AguDeref, deref=p}

    Push r                   -> nullcode {stCode=StMem, inputY=r, aguCode=AguDown, deref=SP, aluCode=Decr, inputX=SP, result=SP}
    Pop r                    -> nullcode {ldCode=LdMem, loadReg=r, aguCode=AguDeref, deref=SP, aluCode=Incr, inputX=SP, result=SP}

    Read (Addr a)            -> nullcode {ioCode=IORead, aguCode=AguImm, addrImm=a}
    Read (Deref p)           -> nullcode {ioCode=IORead, aguCode=AguDeref, deref=p}

    Receive r                -> nullcode {ldCode=LdInp, pcCode=PCWait, loadReg=r}

    TestAndSet (Addr a)      -> nullcode {ioCode=IOTest, aguCode=AguImm, addrImm=a}
    TestAndSet (Deref p)     -> nullcode {ioCode=IOTest, aguCode=AguDeref, deref=p}

    Write j (Addr a)         -> nullcode {ioCode=IOWrite, aguCode=AguImm, addrImm=a, inputY=j}
    Write j (Deref p)        -> nullcode {ioCode=IOWrite, aguCode=AguDeref, deref=p, inputY=j}

    EndProg                  -> nullcode {pcCode=PCJump TRel, immValue=0, sHalted=True}
    Debug _                  -> nullcode


tobit True  = 1
tobit False = 0

-- ============================
alu :: Operator -> Int -> Int -> Int
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

-- ============================
agu :: AguCode -> Int -> Int -> Int
agu aguCode addr derefAddr = case aguCode of
        AguImm   -> addr
        AguDeref -> derefAddr
        AguDown  -> derefAddr - 1
        
-- ============================
load :: [Int] -> LdCode -> (Int, Int, Maybe Int) -> Int
load dmem ldCode (immval, address, input) = case ldCode of
        LdImm -> immval
        LdMem -> dmem !! address
        LdInp -> fromMaybe 0 input

-- ============================
store :: [Int] -> StCode -> Int -> Int -> [Int]
store dmem stCode address value = case stCode of
        StNone -> dmem
        StMem  -> dmem <~ (address, value)

-- ============================
pcUpd :: PCCode -> Int -> Int -> Int -> Maybe a -> Int -> Int
pcUpd pcCode pc cond fromind input immval = case pcCode of
        PCNext     -> succ pc
        PCJump   t -> target t
        PCBranch t -> if cond /= 0 then target t else succ pc
        PCWait     -> if isJust input then succ pc else pc
    where target t = case t of
                TAbs  -> immval
                TRel  -> pc + immval
                TInd  -> fromind

-- ============================
sendOut :: IOCode -> Int -> Int -> Maybe SprockellOut
sendOut ioCode address value = case ioCode of
        IONone    -> Nothing
        IORead    -> Just $ ReadReq address
        IOWrite   -> Just $ WriteReq address value
        IOTest    -> Just $ TestReq address

-- ======================================================================================
-- Putting it all together
sprockell instrs  SprState{..} inputFifo = (sprState, output) where
        pc            = regbank !! fromEnum PC
        MachCode{..}  = decode (instrs !! pc)

        regX          = regbank !! fromEnum inputX
        regY          = regbank !! fromEnum inputY
        aluOutput     = alu aluCode regX regY

        regAddr       = regbank !! fromEnum deref
        address       = agu aguCode addrImm regAddr

        loadValue     = load dmem ldCode (immValue, address, inputFifo)
        dmem'         = store dmem stCode address regY

        nextPC        = pcUpd pcCode pc regX regAddr inputFifo immValue

        regbank'      = regbank    <~ (fromEnum result, aluOutput)
        regbank''     = regbank'   <~ (fromEnum loadReg, loadValue)
        regbank'''    = regbank''  <~ (fromEnum Zero, 0)
        regbank''''   = regbank''' <~ (fromEnum PC, nextPC)

        sprState      = SprState {dmem=dmem',regbank=regbank'''',halted=sHalted}

        -- Managed by System
        output        = sendOut ioCode address regY

-- ==========================================================================================================

xs <: x = xs ++ [x]
xs <+ x = drop 1 xs ++ [x]

xs <~ (i,x) = take i xs ++ [x] ++ drop (i+1) xs         -- TODO: note the effect for i >= length xs
