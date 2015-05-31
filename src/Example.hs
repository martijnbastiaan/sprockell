import Sprockell
import System
import TypesEtc


prog = [
           Const 78 RegA 
         , Put Char RegA
         , EndProg
       ]

main = run 3 prog
