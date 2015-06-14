import Sprockell.System
import Data.Char

{-
    This program demonstrates how to run multiple sprockells at once, each executing
    their own subprogram.

    The Sprockells holding SPID > 0 will read from their respective memory addresses
    until they find a value which is not equal to zero. If it finds such a value, it
    jumps to that memory address. (This example will let them jump to EndProg.)
-}

prog :: [Instruction]
prog = [
           Branch SPID (Rel 3) 
         , Const 10 RegC
         , Write RegC (Addr 1) -- Sprockell 1 must jump to end
         , Write RegC (Addr 2) -- Sprockell 2 must jump to end
         , Jump (Abs 10)       -- Sprockell 0 jumps to end
         -- BEGIN: loop
         , Read (Deref SPID)
         , Receive RegA
         , Compute Equal RegA Zero RegB
         , Branch RegB (Rel (-3))
         -- END: loop
         , Jump (Ind RegA)
         , EndProg
       ]


main = run 3 prog >> putChar '\n'

