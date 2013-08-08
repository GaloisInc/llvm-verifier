module Verifier.LLVM.Debugger
  ( initializeDebugger
  , addBreakpoint
  , breakOnEntry
  ) where

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Simulator.Debugging
import Verifier.LLVM.Simulator.Internals

-- | Break on entry to the function.
breakOnEntry :: (Functor m, Monad m)
             => SymDefine (SBETerm sbe) -> Simulator sbe m ()
breakOnEntry def = addBreakpoint (sdName def) (sdEntry def, 0)
