-- | This module defines the translation from LLVM IR to Symbolic IR.
module SymTranslation where

import qualified Text.LLVM.AST as LLVM
import SymAST

-- | Information about basic block and control-flow graph used during
-- code generation.
data LLVMTranslationInfo = LTI {
    -- Returns post-dominator for given basic block, or none if block does
    -- does not have a post-dominator within this procedure (e.g., terminated with
    -- a ret or unwind statement).
    ltiPostDominator :: LLVM.Ident -> Maybe LLVM.Ident 
  }

liftSymbolic :: LLVMTranslationInfo
             -> LLVM.Define
             -> SymDefine
liftSymbolic _lti _d = undefined
