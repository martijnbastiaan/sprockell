{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypesEtc where
import Components

-- ==========================================================================================================

-- type synonyms for clarity
type Value = Int
type Address = Int
type CodeAddr = Int
type Seed = Int

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
         deriving (Eq,Show,Read,Ord,Enum,Bounded)

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

data Instruction = 
          Compute Operator Reg Reg Reg       -- Compute opCode r0 r1 r2: go to "alu",
                                             --       do "opCode" on regs r0, r1, and put result in reg r2
        | Const Value Reg                    -- Const v r: put value v in register r

        | Branch Reg Target                  -- Branch r t: conditional jump, depending on register r
        | Jump Target                        -- Jump t: jump to target t (absolute, relative, indirect)

        | Load MemAddr Reg                   -- Load (Addr a) r : from "memory a" to "regbank r"
                                             -- Load (Imm  v) r : put "Int v" in "regbank r"
        | Store Reg MemAddr                  -- Store (Addr r) a: from "regbank r" to "memory a"
                                             -- Store (Imm  v) r: put "Int v" in "memory r"
        | Push Reg                           -- push a value on the stack
        | Pop Reg                            -- pop a value from the stack

        | TestAndSet MemAddr                 -- Test address for zero and sets it to one if it is. Returns 1 on success,
                                             -- and 0 on failure. This is an atomic operation; it might therefore be
                                             -- use to implement locks or synchronisation.
        | Read MemAddr                       -- Read from input and put value in regA
        | Receive Reg                        -- Read on its way
        | Write Reg MemAddr                  -- Write content of regA to output

        | EndProg                            -- end of program, deactivates Sprockell. If all sprockells are at
                                             -- this instruction, the simulation will halt.
        | Nop                                -- "No operation".
        | Debug String                       -- No real instruction, for debug purposes.
        deriving (Eq,Show,Read)

-- ==========================================================================================================
-- Internal Sprockell data structures

data PCCode = PCNext
            | PCJump TargetCode
            | PCBranch TargetCode
            | PCWait
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
       , pcCode    :: PCCode       -- next PC determination
       } deriving (Eq,Show)

data SprockellState = SprState
        { regbank   :: !RegBank     -- register bank
        , localMem  :: !LocalMem    -- local data memory
        , halted    :: !Bool
        }

type LocalMem = Memory Value
type RegBank = RegFile Reg Value
type RngState = [Int]
        
type SprockellOut = (Address, SprockelRequest)

data SprockelRequest 
        = ReadReq
        | WriteReq Value
        | TestReq
        deriving (Eq,Show)

type Request = Maybe SprockellOut
type Reply = Maybe Value

newtype SprockellID = SprID Int deriving (Eq,Ord,Enum,Show,Num)
type SharedMem = Memory Value
type InstructionMem = LookupTable Int Instruction

data SystemState = SysState
        { instrs     :: !InstructionMem
        , sprs       :: ![SprockellState]
        , buffersS2M :: ![Buffer Request]
        , buffersM2S :: ![Buffer Reply]
        , queue      :: !(Fifo (SprockellID, SprockellOut))
        , sharedMem  :: !SharedMem
        , rngState   :: !RngState
        }
