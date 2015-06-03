import Sprockell
import System
import TypesEtc
import Data.Char

{-
    This program demonstrates how the ordering of access to the shared memory can vary.

    All the sprockells try to write their own letter to the screen at the same time.
    They will all be succeed, but the order in which this happens is undefined.
-}

loopCount = 10

prog :: [Instruction]
prog = [
           Const (ord 'A') RegA
         , Const (ord 'a' - ord 'A') RegE

         , Compute Add RegA SPID RegB -- sprockell id as ascii character (uppercase)
         , Compute Add RegB RegE RegC -- (lowercase)

         , Const loopCount RegD
         , Const 1 RegE

         , Write RegB (Addr 0x1000000) -- write uppercase letter
         , Write RegC (Addr 0x1000000) -- write lowercase letter
         , Compute Sub RegD RegE RegD
         , Branch RegD (Rel (-3))

         , Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
         , EndProg
       ]


main = run 4 prog

