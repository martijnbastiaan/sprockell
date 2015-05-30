{-# LANGUAGE RecordWildCards #-}

module Sprockell where

import Debug.Trace
import TypesEtc

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| j.kuper@utwente.nl
| October 28, 2012
-------------------------------------------------------------}


initstate ident = SprState {
         regbank = ((replicate regbanksize 0) <~ (fromEnum SPID, ident)) <~ (fromEnum SP, dmemsize - 1)
       , dmem    = replicate dmemsize 0
       , active  = ident == 0 }
        where
            regbanksize =  fromEnum (maxBound::Reg) - fromEnum (minBound::Reg)


nullcode    = MachCode {
                 ldCode      = NoLoad
               , stCode      = NoStore
               , opCode      = Id
               , jmpCode     = TJump
               , spCode      = None
               , ioCode      = IO_None
               , jmpTarget   = TRel
               , immvalueR   = 0
               , immvalueS   = 0
               , fromreg0    = Zero
               , fromreg1    = Zero
               , fromaddr    = 0
               , fromind     = Zero
               , toreg       = Zero
               , toaddr      = 0
               , toind       = Zero
               , memAddr     = 0
               , spDiff      = 1
               , powerCode   = Power_None
               , jumpN       = 1
               }

{-------------------------------------------------------------
| some constants
-------------------------------------------------------------}

dmemsize    = 128 :: Int    -- TODO: memory sizes as yet unused, no "out of memory" yet

incr     =   (+1)
decr     = (+(-1))

tobit True  = 1
tobit False = 0


(i,x) ~> xs  = xs <~ (i,x)


{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

-- ============================
decode :: Int -> Instruction -> MachCode
decode sp instr  = case instr of
    Nop                      -> nullcode

    Compute c i0 i1 i2       -> nullcode {ldCode =LdAlu,  opCode=c, fromreg0=i0, fromreg1=i1, toreg=i2}
    
    Branch (Abs n)           -> nullcode {jmpCode=TBranch, jmpTarget=TAbs, jumpN=n, spCode=Down}
    Branch (Rel n)           -> nullcode {jmpCode=TBranch, jmpTarget=TRel, jumpN=n, spCode=Down}
    Branch (Ind r)           -> nullcode {jmpCode=TBranch, jmpTarget=TInd, fromind=r, spCode=Down}

    Jump   (Abs n)           -> nullcode {jmpCode=TJump, jmpTarget=TAbs, jumpN=n}
    Jump   (Rel n)           -> nullcode {jmpCode=TJump, jmpTarget=TRel, jumpN=n}
    Jump   (Ind r)           -> nullcode {jmpCode=TJump, jmpTarget=TInd, fromind=r}

    Const n r                -> nullcode {ldCode=LdImm, immvalueR=n, toreg=r}

    Load  (Addr n)  r1       -> nullcode {ldCode=LdAddr, fromaddr=n, toreg=r1}
    Load  (Deref r) r1       -> nullcode {ldCode=LdInd, fromind=r, toreg=r1}

    Store r (Addr n)         -> nullcode {stCode=StAddr, fromreg0=r, toaddr=n}
    Store r (Deref r1)       -> nullcode {stCode=StDeref, fromreg0=r, toind=r1}

    Push r                   -> nullcode {stCode=StAddr, fromreg0=r, toaddr=sp-1, spCode=Up}
    Pop r                    -> nullcode {ldCode=LdAddr, fromaddr=sp, toreg =r, spCode=Down}

    Request (Addr n)         -> nullcode {ioCode=IO_Read_Addr, memAddr=n}
    Request (Deref r)        -> nullcode {ioCode=IO_Read_Ind, fromind=r}

    Receive j                -> nullcode {ldCode=LdInp, jmpCode=TWait, toreg=j}

    TestAndSet (Addr i)      -> nullcode {ioCode=IO_Test_Addr, memAddr=i}
    TestAndSet (Deref i)     -> nullcode {ioCode=IO_Test_Ind, fromind=i}

    Write j (Addr i)         -> nullcode {ioCode=IO_Write_Addr, memAddr=i, fromreg0=j}
    Write j (Deref r)        -> nullcode {ioCode=IO_Write_Ind, fromind=r, fromreg0=j}

    Start spr line           -> nullcode {powerCode=Power_Start, fromreg0=spr, fromreg1=line}
    Stop spr                 -> nullcode {powerCode=Power_Stop, fromreg0=spr}

    EndProg                  -> nullcode{jmpCode=TWait}
    Debug _                  -> nullcode


-- ============================
alu :: Operator -> Int -> Int -> Int
alu opCode x y = case opCode of
            Id     -> x
            Incr   -> incr x
            Decr   -> decr x
            Neg    -> -x
            Add    -> x + y
            Sub    -> x - y
            Mul    -> x * y
            Div    -> x `div` y
            Mod    -> x `mod` y
            Equal  -> tobit (x == y)
            NEq    -> tobit (x /= y)
            Gt     -> tobit (x > y)
            Lt     -> tobit (x < y)
            And    -> x * y
            Or     -> x `max` y
            Not    -> 1 - x

-- ============================
load :: [Int] -> LdCode -> Reg -> (Int, Int, Int, Int, Maybe Int) -> [Int]
load regbank ldCode reg (immvalueR, mval, toAddrVal, aluOutput, input) = regbank <~ (fromEnum reg, v)
        where
            v =  case ldCode of
                NoLoad  -> 0
                LdImm   -> immvalueR
                LdInd   -> mval
                LdAddr  -> toAddrVal
                LdAlu   -> aluOutput
                LdInp   -> case input of
                             Just x  -> x
                             Nothing -> 0

-- ============================
store :: [Int] -> StCode -> Int -> (Int,Int,Int) -> [Int]
store dmem stCode toaddr (immvalueS, toAddr, x) = dmem'
        where
          dmem' =  case stCode of
                NoStore -> dmem
                StDeref -> dmem <~ (toAddr, x)
                StAddr  -> dmem <~ (toaddr, x)


-- ============================
pcUpd :: JmpCode -> TargetCode -> Int -> Int -> Int -> Maybe a -> Int -> Int
pcUpd jmpCode jmpTarget pc x fromind input immvalueR = n
        where
            n = case jmpCode of
                   TJump   -> target
                   TBranch -> if x == 1 then target else incr pc
                   TWait   -> case input of
                             Just _  -> target
                             Nothing -> pc

            target = case jmpTarget of
                        TAbs -> immvalueR
                        TRel -> pc + immvalueR
                        TInd -> fromind

-- ============================
spUpd :: SPCode -> Int -> Int -> Int
spUpd spCode sp spDiff    = case spCode of
            Up      -> sp - spDiff
            Down    -> sp + spDiff
            None    -> sp

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

sprockell :: SprFunc

sprockell ident  instrs  SprState{..} input = sprstate
        where
          MachCode{..}  = decode (regbank !! fromEnum SP) (instrs !! fromEnum PC)

          toAddr        = regbank !! toaddr
          toIndAddr     = regbank !! (fromEnum toind)

          reg0          = regbank !! (fromEnum fromreg0)
          reg1          = regbank !! (fromEnum fromreg1)
          aluOutput     = alu opCode reg0 reg1

          memval        = dmem !! fromaddr
          indMemval     = dmem !! (regbank !! fromEnum fromind)

          regbank'      = load regbank ldCode toreg (immvalueR, memval, indMemval, aluOutput, input)
          regbank''     = regbank'   <~ (fromEnum Zero, 0)
          regbank'''    = regbank''  <~ (fromEnum SP, spUpd spCode spDiff (regbank !! fromEnum SP))
          regbank''''   = regbank''' <~ (fromEnum PC, pcUpd jmpCode jmpTarget (regbank !! fromEnum PC) reg0 indMemval input jumpN)

          dmem'         = store dmem stCode toaddr (immvalueS, toAddr, reg0)

          -- Managed by System
          output        = sendOut ioCode (memAddr, indMemval, reg0)
          power         = sendPower powerCode (reg0, reg1)

          sprstate      = (SprState {dmem=dmem',regbank=regbank'''',active=True}, output, power)
