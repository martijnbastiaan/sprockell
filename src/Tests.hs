import Sprockell
import System
import TypesEtc
import Debug.Trace

type TestSuite = (String, Int, [Instruction], (SystemState -> String))

getRegs regbank = map ((regbank !!) . fromEnum)

-- Can we write to registers and does zero stay zero? --
writeRegProg = [Const 10 RegA, Const 11 RegB, Const 15 Zero, EndProg]
writeRegSuite = ("RegTest", 1, writeRegProg, writeRegTest)
writeRegTest ([Sprockell _ _ SprState{..}], _, _, _, _, _) 
        | getRegs regbank [RegA, RegB, Zero] == [10, 11, 0] = "OK"

-- Does computing work? --
computeProg = [Const 3 RegA, Const 2 RegB, Compute Mul RegA RegB RegC, EndProg]
computeSuite = ("ComputeTest", 1, computeProg, computeTest)
computeTest ([Sprockell _ _ SprState{..}], _, _, _, _, _) 
        | getRegs regbank [RegA, RegB, RegC] == [3, 2, 6] = "OK"

-- Indirect Load --
indirectLoadSuite = ("IndirectLoadTest", 1, indirectLoadProg, indirectLoadTest)
indirectLoadProg = [
          Const 2 RegA
        , Const 3 RegB
        , Store RegA (Addr 3)
        , Load (Deref RegB) RegC
        , EndProg
        ] 

indirectLoadTest ([Sprockell _ _ SprState{..}], _, _, _, _, _) 
         | getRegs regbank [RegA, RegB, RegC] == [2, 3, 2] = "OK"

-- Indirect Store --
indirectStoreSuite = ("IndirectStoreTest", 1, indirectStoreProg, indirectStoreTest)
indirectStoreProg = [
          Const 2 RegA
        , Const 3 RegB
        , Store RegA (Deref RegB)
        , Load (Addr 3) RegC
        , EndProg
        ] 

indirectStoreTest ([Sprockell _ _ SprState{..}], _, _, _, _, _) 
         | getRegs regbank [RegA, RegB, RegC] == [2, 3, 2] = "OK"


-- Running test logic --
runSuite :: TestSuite -> IO ()
runSuite (name, nSprockells, prog, test) = do
    putStr name >> putStr " (n=" >> putStr (show nSprockells) >> putStr "): "
    run nSprockells prog >>= return . test >>= putStr
    putChar '\n'
    return ()
    
main = do
    runSuite writeRegSuite
    runSuite computeSuite
    runSuite indirectLoadSuite
    runSuite indirectStoreSuite
    return ()

