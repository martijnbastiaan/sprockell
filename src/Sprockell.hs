{-# LANGUAGE RecordWildCards #-}

module Sprockell where

import Data.Bits
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


initstate ident = SprState {regbank = regvalues, dmem = replicate dmemsize 0, active  = ident == 0 }
    where
        regvalues = ((replicate regbanksize 0) <~ (fromEnum SPID, ident)) <~ (fromEnum SP, dmemsize - 1)
        regbanksize =  fromEnum (maxBound::Reg) - fromEnum (minBound::Reg)


nullcode    = MachCode {
                 ldCode      = LdImm
               , stCode      = NoStore
               , opCode      = Or
               , jmpCode     = TNext
               , ioCode      = IO_None
               , jmpTarget   = TAbs
               , immvalue    = 0
               , fromreg0    = Zero
               , fromreg1    = Zero
               , toreg       = Zero
               , loadreg     = Zero
               , addr        = 0
               , deref       = Zero
               , powerCode   = Power_None
               }

{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

-- ============================
decode :: Int -> Instruction -> MachCode
decode sp instr  = case instr of
    Nop                      -> nullcode

    Compute c r0 r1 r2       -> nullcode {opCode=c, fromreg0=r0, fromreg1=r1, toreg=r2}
    
    Branch r0 (Abs n)        -> nullcode {jmpCode=TBranch, jmpTarget=TAbs, immvalue=n, fromreg0=r0}
    Branch r0 (Rel n)        -> nullcode {jmpCode=TBranch, jmpTarget=TRel, immvalue=n, fromreg0=r0}
    Branch r0 (Ind i)        -> nullcode {jmpCode=TBranch, jmpTarget=TInd, deref=i   , fromreg0=r0}

    Jump   (Abs n)           -> nullcode {jmpCode=TJump, jmpTarget=TAbs, immvalue=n}
    Jump   (Rel n)           -> nullcode {jmpCode=TJump, jmpTarget=TRel, immvalue=n}
    Jump   (Ind r)           -> nullcode {jmpCode=TJump, jmpTarget=TInd, deref=r}

    Const n r                -> nullcode {ldCode=LdImm, immvalue=n, loadreg=r}

    Load  (Addr a)  r        -> nullcode {ldCode=LdAddr, addr=a, loadreg=r}
    Load  (Deref p) r        -> nullcode {ldCode=LdInd, deref=p, loadreg=r}

    Store r (Addr a)         -> nullcode {stCode=StAddr, fromreg1=r, addr=a}
    Store r (Deref p)        -> nullcode {stCode=StDeref, fromreg1=r, deref=p}

    Push r                   -> nullcode {stCode=StAddr, fromreg1=r, addr=sp-1, opCode=Decr, fromreg0=SP, toreg=SP}
    Pop r                    -> nullcode {ldCode=LdAddr, addr=sp, loadreg=r   , opCode=Incr, fromreg0=SP, toreg=SP}

    Request (Addr a)         -> nullcode {ioCode=IO_Read_Addr, addr=a}
    Request (Deref p)        -> nullcode {ioCode=IO_Read_Ind, deref=p}

    Receive r                -> nullcode {ldCode=LdInp, jmpCode=TWait, toreg=r}

    TestAndSet (Addr a)      -> nullcode {ioCode=IO_Test_Addr, addr=a}
    TestAndSet (Deref p)     -> nullcode {ioCode=IO_Test_Ind, deref=p}

    Write j (Addr a)         -> nullcode {ioCode=IO_Write_Addr, addr=a, fromreg1=j}
    Write j (Deref p)        -> nullcode {ioCode=IO_Write_Ind, deref=p, fromreg1=j}

    Start spr line           -> nullcode {powerCode=Power_Start, fromreg0=spr, fromreg1=line}
    Stop spr                 -> nullcode {powerCode=Power_Stop, fromreg0=spr}

    EndProg                  -> nullcode{jmpCode=TWait}
    Debug _                  -> nullcode


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
            RShift -> shiftL x y
            Xor    -> x `xor` y
    where
        tobit True  = 1
        tobit False = 0

-- ============================
load :: [Int] -> LdCode -> (Int, Int, Int, Maybe Int) -> Int
load dmem ldCode (immvalue, addr, derefAddr, input) = case ldCode of
        LdImm   -> immvalue
        LdAddr  -> dmem !! addr
        LdInd   -> dmem !! derefAddr
        LdInp   -> case input of
                        Just x  -> x
                        Nothing -> 0

-- ============================
store :: [Int] -> StCode -> (Int, Int,Int) -> [Int]
store dmem stCode (addr, derefAddr, x) = case stCode of
        NoStore -> dmem
        StAddr  -> dmem <~ (addr, x)
        StDeref -> dmem <~ (derefAddr, x)


-- ============================
pcUpd :: JmpCode -> TargetCode -> Int -> Int -> Int -> Maybe a -> Int -> Int
pcUpd jmpCode jmpTarget pc cond fromind input immval = pc'
        where
            pc' = case jmpCode of
                   TNext   -> succ pc
                   TJump   -> target
                   TBranch -> if cond /= 0 then target else succ pc
                   TWait   -> case input of
                             Just _  -> succ pc
                             Nothing -> pc

            target = case jmpTarget of
                        TAbs  -> immval
                        TRel  -> pc + immval
                        TInd  -> fromind

-- ============================
sendOut :: IOCode -> (Int, Int, Int) -> Maybe SprockellOut
sendOut ioCode (memaddr,fromind,value) = case ioCode of
                    IO_None        -> Nothing
                    IO_Read_Addr   -> Just $ ReadReq  memaddr
                    IO_Read_Ind    -> Just $ ReadReq  fromind
                    IO_Write_Addr  -> Just $ WriteReq memaddr value
                    IO_Write_Ind   -> Just $ WriteReq fromind value
                    IO_Test_Addr   -> Just $ TestReq  memaddr
                    IO_Test_Ind    -> Just $ TestReq  fromind

sendPower :: PowerCode -> (Int, Int) -> Maybe PowerOut
sendPower powerCode (sprockell, instr) = case powerCode of
                    Power_None  -> Nothing
                    Power_Start -> Just $ StartReq sprockell instr
                    Power_Stop  -> Just $ StopReq sprockell

-- ======================================================================================
-- Putting it all together
sprockell ident instrs  SprState{..} input = sprstate
        where
          MachCode{..}  = decode (regbank !! fromEnum SP) (instrs !! fromEnum PC)

          reg0          = regbank !! (fromEnum fromreg0)
          reg1          = regbank !! (fromEnum fromreg1)
          aluOutput     = alu opCode reg0 reg1

          derefAddr     = regbank !! (fromEnum deref)

          loadValue     = load dmem ldCode (immvalue, addr, derefAddr, input)
          nextPC        = pcUpd jmpCode jmpTarget (regbank !! fromEnum PC) reg0 derefAddr input immvalue
          
          regbank'      = regbank    <~ (fromEnum toreg, aluOutput)
          regbank''     = regbank'   <~ (fromEnum loadreg, loadValue)
          regbank'''    = regbank''  <~ (fromEnum Zero, 0)
          regbank''''   = regbank''' <~ (fromEnum PC, nextPC)

          dmem'         = store dmem stCode (addr, derefAddr, reg1)

          -- Managed by System
          output        = sendOut ioCode (addr, derefAddr, reg1)
          power         = sendPower powerCode (reg0, reg1)

          sprstate      = (SprState {dmem=dmem',regbank=regbank'''',active=True}, output, power)

-- ==========================================================================================================

xs <: x = xs ++ [x]
xs <+ x = drop 1 xs ++ [x]

xs <~ (i,x) = take i xs ++ [x] ++ drop (i+1) xs         -- TODO: note the effect for i >= length xs
