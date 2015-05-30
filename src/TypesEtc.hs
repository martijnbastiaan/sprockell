{-# LANGUAGE FlexibleInstances #-}

module TypesEtc where

import Data.List
import Debug.Trace

-- ==========================================================================================================
-- Sprockell instructions
data Reg = Zero
         | PC
         | SP
         | SPID
         | RegA
         | RegB
         | RegC
         | RegD
         | RegE
         deriving (Eq,Show,Ord,Enum,Bounded)

data MemAddr = Addr Int
             | Deref Reg
             deriving (Eq,Show)

data Target = Abs Int
            | Rel Int
            | Ind Reg
            deriving (Eq,Show)

data Operator   = 
                -- unary operations
                Incr | Decr | Neg | Not
                -- binary operations
                | Add  | Sub | Mul  | Div | Mod 
                -- comparision operations
                | Equal | NEq | Gt | Lt | GtE | LtE
                -- logical/binary operations
                | And | Or
                deriving (Eq,Show)


data Instruction = 
          -- Compute opCode r0 r1 r2: go to "alu",
          -- do "opCode" on regs r0, r1, and put result in reg r2
          Compute Operator Reg Reg Reg  
                                         
        | Const Int Reg

        | Branch Reg Target
        | Jump Target

        | Load MemAddr Reg                -- Load (Addr a) r : from "memory a" to "regbank r"
                                          -- Load (Imm  v) r : put "Int v" in "regbank r"
        | Store Reg MemAddr               -- Store (Addr r) a: from "regbank r" to "memory a"
                                          -- Store (Imm  v) r: put "Int v" in "memory r"
        | Push Reg                        -- push a value on the stack
        | Pop Reg                         -- pop a value from the stack

        | TestAndSet MemAddr    -- Test address for zero and sets it to one if it is. Returns 1 on success,
                    -- and 0 on failure. This is an atomic operation; it might therefore be
                    -- use to implement locks or synchronisation.
        | Request MemAddr    -- Read from input and put value in regA
        | Receive Reg        -- Read on its way
        | Write Reg MemAddr    -- Write content of regA to output

        | Start Reg Reg        -- Start Sprockell `regA` at instruction `regB`. You can use this on
                    -- running Sprockells; keep in mind it only changes the program counter,
                    -- while the rest of the data (registers, oustanding I/O requests) are 
                    -- not touched at all. Here be dragons!

        | Stop Reg        -- Stop Sprockell `regA`
 
        | EndProg        -- end of program, deactivates Sprockell. If all sprockells are at
                    -- this instruction, the simulation will halt.
        | Nop            -- "No operation"
        | Debug String
        deriving (Eq,Show)


-- ==========================================================================================================
-- Internal Sprockell data structures

data TargetCode = TAbs
                | TRel
                | TInd
                deriving (Eq,Show)

data LdCode = LdImm
            | LdAddr
            | LdInd
            | LdInp 
            deriving (Eq,Show)

data StCode = NoStore
            | StAddr
            | StDeref
            deriving (Eq,Show)

data JmpCode = TNext
             | TJump
             | TBranch
             | TWait
             deriving (Eq,Show)

data IOCode = IO_None
            | IO_Read_Addr
            | IO_Read_Ind
            | IO_Write_Addr
            | IO_Write_Ind
            | IO_Test_Addr
            | IO_Test_Ind
            deriving (Eq,Show)

data PowerCode = Power_None
               | Power_Start
               | Power_Stop
               deriving (Eq,Show)

data MachCode = MachCode { 
         ldCode    :: LdCode       -- 0/1: load from dmem to rbank?
       , stCode    :: StCode       -- storeCode
       , opCode    :: Operator -- opCode
       , ioCode    :: IOCode    -- code whether to send a read or werite message
       , immvalue  :: Int          -- value from Immediate
       , fromreg0  :: Reg          -- ibid, first parameter of Compute
       , fromreg1  :: Reg          -- ibid, second parameter of Compute
       , toreg     :: Reg          -- ibid, third parameter of Compute
       , loadreg   :: Reg          -- where to load result is writen to
       , addr      :: Int          -- address in dmem
       , deref     :: Reg
       , jmpTarget :: TargetCode
       , jmpCode   :: JmpCode      -- 0/1: indicates a jump
       , powerCode :: PowerCode     -- indicates whether another Sprockell core should be started
        }
            deriving (Eq,Show)



data SprState = SprState {
                    regbank   :: [Int]        -- register bank
                  , dmem      :: [Int]        -- main memory, data memory
                  , active    :: Bool
                  }

              | RtrState RtrMessage
                    deriving (Eq,Show)


data SprockellOut = 
            ReadReq  Int            -- ReadReq   adres
          | WriteReq Int Int        -- WriteReq  adres value
          | TestReq  Int            -- TestReq   adres
             deriving (Eq,Show)

data PowerOut =
                StartReq Int Int
              | StopReq Int 
                 deriving (Eq,Show)

type RtrMessage    = Maybe (Int,Int)        -- Just (m,a): m is nr of processor, a is content of message

type Stack       = [Int]

type Request     = Maybe SprockellOut
type Reply     = Maybe Int


type SprFunc  = Int -> [Instruction] -> SprState -> Maybe Int -> (SprState, Maybe SprockellOut, Maybe PowerOut)


data Sprockell = Sprockell Int SprFunc [Instruction] SprState

--                 Sprockell outputs    mem   seq
type ShMemState = ([(Int,SprockellOut)],[Int], Int)
type ShMemFunc  = ShMemState -> [Request] -> (ShMemState,[Reply])

data ShMem = ShMem ShMemFunc ShMemState

type SystemState = ([Sprockell], [[Request]], [[Reply]], ShMem)


-- ==========================================================================================================
-- Clock for simulation of the Sprockell

data Tick = Tick deriving (Eq,Show)
clock = Tick : clock
