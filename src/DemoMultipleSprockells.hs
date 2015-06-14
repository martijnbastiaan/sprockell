import Sprockell.System

{-
    This program demonstrates how to run multiple sprockells at once, each executing
    their own subprogram.

    The Sprockells holding SPID > 0 will read from their respective memory addresses
    until they find a value which is not equal to zero. If it finds such a value, it
    jumps to that memory address. (This example will let them jump to EndProg.)
-}

prog :: [Instruction]
prog = [
           Branch SPID (Rel 5)
         , Const 11 RegC
         , Write RegC (Addr 1) -- Sprockell 1 must jump to second end
         , Write RegC (Addr 2) -- Sprockell 2 must jump to second end
         , Jump (Abs 10)       -- Sprockell 0 jumps to first end
         -- BEGIN: loop
         , Read (Deref SPID)
         , Receive RegA
         , Compute Equal RegA Zero RegB
         , Branch RegB (Rel (-3))
         -- END: loop
         , Jump (Ind RegA)

         -- 10:
         , EndProg

         -- 11: Sprockells 1 and 2 are sent here
         , EndProg
       ]


main = runDebug debugEndProg 3 prog >> putChar '\n'

-- This debug function show a message when a Sprockell reaches an EndProg instruction.
debugEndProg SysState{sprs=sprs,instrs=instrs} = concat $ map isHalting sprs
    where
        isHalting SprState{regbank=regs,halted=halted}
            | not halted && instrs!pc == EndProg
                = "EndProg on Sprockell " ++ show spid ++ " at addr " ++ show pc ++ "\n"
            | otherwise = ""
            where
                pc   = regs ! PC
                spid = regs ! SPID
