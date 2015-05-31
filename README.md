# Sprockell
Sprockell is a **S**imple **Pr**ocessor in Has**kell**. It was originally written by Jan Kuper at the University of Twente. It has later been extended to allow multiple Sprockells to be ran at once, communicating via shared memory. This version is internally been dubbed Smockell :-)

# Features
* Simple arithmetic
* Simple I/O (stdin / stdout)
* Branches / jumps
* Stack
* Local memory
* Shared memory
 
# Documentation
See the [wiki](https://github.com/martijnbastiaan/sprockell/wiki).

# Running
Clone the repository, compile `System.hs` using `ghc` and run it from the command line. A (really) simple program runs and terminates.

Of course, you can compile your own program:

```haskell
module Program where

import Sprockell
import System
import TypesEtc

prog = [
    Const 8 regA
    Const 9 regB
    Compute Add RegA RegB RegC
   ]

run 1 prog
```

Where `1` is the amount of Sprockells you want to deploy.

```bash
ghc Program.hs
./Program
```

(Of course, this simple program doesn't produce any output. You'd need the `Put` instruction for that.)

# Debugging
See the wiki on [debugging](https://github.com/martijnbastiaan/sprockell/wiki/debugging).
