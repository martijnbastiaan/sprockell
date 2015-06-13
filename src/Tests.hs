import Sprockell.System
import Debug.Trace

type TestSuite = (String, Int, [Instruction], (SystemState -> String))

getRegs sysState spid regs = getRegs' (regbank ((sprs sysState) !! spid)) regs
    where getRegs' rs = map (rs !)

-- Can we write to registers and does zero stay zero? --
writeRegProg = [Const 10 RegA, Const 11 RegB, Const 15 Zero, EndProg]
writeRegSuite = ("RegTest", 1, writeRegProg, writeRegTest)

writeRegTest sysState
        | getRegs sysState 0 [RegA, RegB, Zero] == [10, 11, 0] = "OK"

-- Does computing work? --
computeProg = [Const 3 RegA, Const 2 RegB, Compute Mul RegA RegB RegC, EndProg]
computeSuite = ("ComputeTest", 1, computeProg, computeTest)

computeTest sysState
        | getRegs sysState 0  [RegA, RegB, RegC] == [3, 2, 6] = "OK"

-- Indirect Load --
indirectLoadSuite = ("IndirectLoadTest", 1, indirectLoadProg, indirectLoadTest)
indirectLoadProg = [
          Const 2 RegA
        , Const 3 RegB
        , Store RegA (Addr 3)
        , Load (Deref RegB) RegC
        , EndProg
        ] 

indirectLoadTest sysState
         | getRegs sysState 0 [RegA, RegB, RegC] == [2, 3, 2] = "OK"

-- Write to Zero
writeZeroProg = [Const 2 Zero, Compute Add Zero RegA RegA, EndProg]
writeZeroSuite = ("ZeroTest", 1, writeZeroProg, writeZeroTest)

writeZeroTest sysState
        | getRegs sysState 0 [RegA, Zero] == [0, 0] = "OK"

-- Indirect Store --
indirectStoreSuite = ("IndirectStoreTest", 1, indirectStoreProg, indirectStoreTest)
indirectStoreProg = [
          Const 2 RegA
        , Const 3 RegB
        , Store RegA (Deref RegB)
        , Load (Addr 3) RegC
        , EndProg
        ] 

indirectStoreTest sysState
         | getRegs sysState 0 [RegA, RegB, RegC] == [2, 3, 2] = "OK"

-- Check the value of local mem which was not previously written to --
unwrittenLocalSuite = ("UnwrittenLocalTest", 1, unwrittenLocalProg, unwrittenLocalTest)
unwrittenLocalProg = [
          Const 2 RegA
        , Const 3 RegB
        , Load (Addr 0) RegA
        , Load (Deref RegB) RegB
        , EndProg
        ]
unwrittenLocalTest sysState
         | getRegs sysState 0 [RegA, RegB] == [0, 0] = "OK"

-- Check the value of shared mem which was not previously written to --
unwrittenSharedSuite = ("UnwrittenSharedTest", 1, unwrittenSharedProg, unwrittenSharedTest)
unwrittenSharedProg = [
          Const 2 RegA
        , Const 3 RegB
        , Read (Addr 0)
        , Receive RegA
        , Read (Deref RegB)
        , Receive RegB
        , EndProg
        ]
unwrittenSharedTest sysState
         | getRegs sysState 0 [RegA, RegB] == [0, 0] = "OK"



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
    runSuite writeZeroSuite
    runSuite indirectLoadSuite
    runSuite indirectStoreSuite
    runSuite unwrittenLocalSuite
    runSuite unwrittenSharedSuite
    return ()

