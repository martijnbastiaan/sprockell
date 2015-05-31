{-# LANGUAGE FlexibleInstances #-}

module TypesEtc where

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
         deriving (Eq,Show,Read,Ord,Enum,Bounded)

data MemAddr = Addr Int
             | Deref Reg
             deriving (Eq,Show,Read)

data Target = Abs Int
            | Rel Int
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

data PutType = Int | Char
             deriving (Eq,Show,Read)

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
        | Read MemAddr    -- Read from input and put value in regA
        | Receive Reg        -- Read on its way
        | Write Reg MemAddr    -- Write content of regA to output
        | Put PutType Reg
        | Get

        | Start Reg Reg        -- Start Sprockell `regA` at instruction `regB`. You can use this on
                    -- running Sprockells; keep in mind it only changes the program counter,
                    -- while the rest of the data (registers, oustanding I/O requests) are 
                    -- not touched at all. Here be dragons!

        | Stop Reg        -- Stop Sprockell `regA`
 
        | EndProg        -- end of program, deactivates Sprockell. If all sprockells are at
                    -- this instruction, the simulation will halt.
        | Nop            -- "No operation"
        | Debug String
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
            | IOPutChar
            | IOPutInt
            | IOGet
            deriving (Eq,Show)

data PowerCode = PowerNone
               | PowerStart
               | PowerStop
               deriving (Eq,Show)

data MachCode = MachCode
       { ldCode    :: LdCode       -- source of load results
       , stCode    :: StCode       -- store command
       , aguCode   :: AguCode      -- address calculation 
       , aluCode   :: Operator     -- arithmetic operation
       , ioCode    :: IOCode       -- communication with the rest of the system
       , immValue  :: Int          -- value from Immediate
       , inputX    :: Reg          -- first input register
       , inputY    :: Reg          -- seconde input register
       , result    :: Reg          -- alu result register
       , loadReg   :: Reg          -- where to load results are written to
       , addrImm   :: Int          -- address constant
       , deref     :: Reg          -- address register
       , pcCode    :: PCCode       -- next PC determination
       , powerCode :: PowerCode    -- indicates whether another Sprockell core should be started
       } deriving (Eq,Show)

data SprState = SprState
        { regbank   :: [Int]        -- register bank
        , dmem      :: [Int]        -- local data memory
        , active    :: Bool
        }

data SprockellOut 
        = ReadReq    Int            -- ReadReq   adres
        | WriteReq   Int Int        -- WriteReq  adres value
        | TestReq    Int            -- TestReq   adres
        | PutIntReq  Int
        | PutCharReq Int
        | GetReq
        deriving (Eq,Show)

data PowerOut = StartReq Int Int
              | StopReq Int 
              deriving (Eq,Show)

type Request = Maybe SprockellOut
type Reply = Maybe Int

data Sprockell = Sprockell Int [Instruction] SprState

--                 Sprockell outputs    mem   seq
type ShMemState = ([(Int,SprockellOut)],[Int], Int)

data ShMem = ShMem ShMemState

type SystemState = ([Sprockell], [[Request]], [[Reply]], ShMem)

