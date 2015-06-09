{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

-- Note that it never prints "First shared memaddr equals 5": all sprockells
-- are terminated before the shared memory gets a chance to write it.
prog :: [Instruction]
prog = [
           Const 78 RegA 
         , Const 10 RegB
         , Const 5  RegC
         , Write RegA (Addr 0x1000000) -- write to stdout using explicit address
         , Write RegB stdio            -- or using the alias
         , Write RegC (Addr 0)
         -- If we add some Nop's to delay the EndProg
         --  then the shared memory has time to handle all the writes.
         -- And the debug message will be printed.
         --, Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
         , EndProg
       ]

debug :: SystemState -> String
debug SysState{..} | (sharedMem !!! 0) == 5 = "First shared memaddr equals 5.\n"
debug _ = "Not 5\n"

main = runDebug debug 3 prog
