import Sprockell
import System
import TypesEtc


-- Note that it never prints "First shared memaddr equals 5": all sprockells
-- are terminated before the shared memory gets a chance to write it.
prog :: [Instruction]
prog = [
           Const 78 RegA 
         , Const 10 RegB
         , Const 5  RegC
         , Write RegA (Addr 0x100)
         , Write RegB (Addr 0x100)
         , Write RegC (Addr 0)
         , EndProg
       ]

debug :: SystemState -> String
debug (_, _, _, _, (5:xs), _) = "First shared memaddr equals 5.\n"
debug _ = ""

main = runDebug debug 3 prog
