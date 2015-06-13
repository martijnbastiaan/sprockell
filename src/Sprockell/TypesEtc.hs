module Sprockell.TypesEtc where

import Data.Int (Int32)
import Data.Array (Ix)
import Sprockell.Components

-- ==========================================================================================================

-- Sprockell instructions
data Instruction = 
          Compute Operator Reg Reg Reg       -- Compute opCode r0 r1 r2: go to "alu",
                                             --       do "opCode" on regs r0, r1, and put result in reg r2
        | Const Value Reg                    -- Const v r: put value v in register r

        | Branch Reg Target                  -- Branch r t: conditional jump, depending on register r
        | Jump Target                        -- Jump t: jump to target t (absolute, relative, indirect)

        | Load MemAddr Reg                   -- Load (Addr a) r : from "memory a" to "regbank r"
                                             -- Load (Deref p) r : from memory indexed by register p to "r"
        | Store Reg MemAddr                  -- Store (Addr r) a: from "regbank r" to "memory a"
                                             -- Store (Deref p) r: from "r" to memory indexed by registers p
        | Push Reg                           -- push a value on the stack
        | Pop Reg                            -- pop a value from the stack

        | Read MemAddr                       -- Send read request for an external address
        | Receive Reg                        -- Wait for a reply of a request and save it in register
        | Write Reg MemAddr                  -- Write content of reg to an external address
        | TestAndSet MemAddr                 -- Request a test on address for 0 and sets it to 1 if it is.
                                             -- Reply will contain 1 on success, and 0 on failure.
                                             -- This is an atomic operation; it might therefore be
                                             -- used to implement locks or synchronisation.

        | EndProg                            -- end of program, deactivates Sprockell. If all sprockells are at
                                             -- this instruction, the simulation will halt.
        | Nop                                -- "No operation".
        | Debug String                       -- No real instruction, for debug purposes.
        deriving (Eq,Show,Read)

data Reg = Zero                              -- Read only zero value
         | PC                                -- Program counter
         | SP                                -- Stack pointer used by Push and Pop
         | SPID                              -- Sprockell identifier
         | RegA
         | RegB
         | RegC
         | RegD
         | RegE
         deriving (Eq,Show,Read,Ord,Enum,Bounded,Ix)

data MemAddr = Addr Address
             | Deref Reg
             deriving (Eq,Show,Read)

data Target = Abs CodeAddr
            | Rel CodeAddr
            | Ind Reg
            deriving (Eq,Show,Read)

data Operator = Add  | Sub | Mul  | Div | Mod 
              -- comparision operations
              |  Equal | NEq | Gt | Lt | GtE | LtE
              -- logical/binary operations
              | And | Or | Xor | LShift | RShift
              -- Internal
              | Decr | Incr
              deriving (Eq,Show,Read)

-- type synonyms for clarity
type Value = Int32
type Address = Int32
type CodeAddr = Int32

-- ==========================================================================================================
-- Internal Sprockell data structures

data CondCode = CFalse
              | CTrue
              | CReg
              | CWait
              deriving (Eq,Show)

data TargetCode = TAbs
                | TRel
                | TInd
                deriving (Eq,Show)

data AguCode = AguImm
             | AguDeref
             | AguDown
             deriving (Eq,Show)

data LdCode = LdImm
            | LdMem
            | LdInp 
            deriving (Eq,Show)

data StCode = StNone
            | StMem
            deriving (Eq,Show)

data IOCode = IONone
            | IORead
            | IOWrite
            | IOTest
            deriving (Eq,Show)

data MachCode = MachCode
        { ldCode    :: LdCode       -- source of load results
        , stCode    :: StCode       -- store command
        , aguCode   :: AguCode      -- address calculation 
        , aluCode   :: Operator     -- arithmetic operation
        , ioCode    :: IOCode       -- communication with the rest of the system
        , immValue  :: Value        -- value from Immediate
        , inputX    :: Reg          -- first input register
        , inputY    :: Reg          -- seconde input register
        , result    :: Reg          -- alu result register
        , loadReg   :: Reg          -- where to load results are written to
        , addrImm   :: Address      -- address constant
        , deref     :: Reg          -- address register
        , condCode  :: CondCode     -- branching condition
        , target    :: TargetCode   -- branch target computation
        } deriving (Eq,Show)

data SprockellState = SprState
        { regbank   :: !RegBank     -- register bank
        , localMem  :: !LocalMem    -- local data memory
        , halted    :: !Bool
        }

type InstructionMem = LookupTable CodeAddr Instruction
type LocalMem = Memory Value
type RegBank = RegFile Reg Value

type Reply = Value
type Request = (Address, RequestKind)
data RequestKind = ReadReq
                 | WriteReq Value
                 | TestReq
                 deriving (Eq,Show)
