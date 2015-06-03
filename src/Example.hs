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
         , Write RegA (Addr 0x1000000)
         , Write RegB (Addr 0x1000000)
         , Write RegC (Addr 0)
         -- If we add some Nop's to delay the EndProg
         --  then the shared memory has time to handle all the writes.
         -- And the debug message will be printed.
         -- , Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
         , EndProg
       ]

debug :: SystemState -> String
debug SysState{ sharedMem = 5:xs } = "First shared memaddr equals 5.\n"
debug _ = ""

main = runDebug debug 3 prog
