import Sprockell
import System
import TypesEtc


prog :: [Instruction]
prog = [
           Const 78 RegA 
         , Put Char RegA
         , EndProg
       ]

debug :: SystemState -> String
debug (_, _, _, ShMem (_, (5:xs), _)) = "First shared memaddr equals 5."
debug _ = ""

main = runDebug debug 3 prog
