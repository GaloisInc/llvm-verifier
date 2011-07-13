{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module defines the translation from LLVM IR to Symbolic IR.
module SymTranslation where

import Control.Monad.State
import Data.Map (Map)
import qualified Text.LLVM.AST as LLVM
import Text.LLVM.AST (Stmt(..))
import Text.PrettyPrint.HughesPJ

import SymAST

-- | Information about basic block and control-flow graph used during
-- code generation.
data LLVMTranslationInfo = LTI {
    -- Returns post-dominator for given basic block, or none if block does
    -- does not have a post-dominator within this procedure (e.g., terminated with
    -- a ret or unwind statement).
    ltiPostDominator :: LLVM.Ident -> Maybe LLVM.Ident 
  }


-- | This function is called whenever lifting fails due to an internal error.
liftError :: Doc -> a
liftError d = error (render d)

data LiftState = LS 
  { lsNextBlock :: SymBlockID
  , lsBlockDefinitions :: Map SymBlockID SymBlock
  }

newtype BlockGenerator a = BG (State LiftState a)
  deriving (Functor, Monad)

-- | Returns new block identifier.
newBlockID :: BlockGenerator SymBlockID
newBlockID = undefined

-- | Define block with given identifier.
defineBlock :: SymBlockID -> SymBlock -> BlockGenerator ()
defineBlock _id _block = undefined

-- | Returns symbolic block for LLVM block with given identifier.
type SymBlockFn = LLVM.Ident -> SymBlockID

-- | Lift nonterminal statement
liftNT :: LLVM.Stmt -> SymBlockFn -> SymStmt
liftNT (Result id (LLVM.Arith op tpv1 v2)) _   = Assign id (Arith op tpv1 v2)
liftNT (Result id (LLVM.Bit   op tpv1 v2)) _   = Assign id (Bit op tpv1 v2)
liftNT (Result id (LLVM.Conv  op tpv1 v2)) _   = Assign id (Conv op tpv1 v2)
liftNT (Result id (LLVM.Alloca tp mtpv mi)) _  = Assign id (Alloca tp mtpv mi)
liftNT (Result id (LLVM.Load tpv)) _           = Assign id (Load tpv)
liftNT (Effect    (LLVM.Store a v)) _          = Store a v
liftNT (Result id (LLVM.ICmp op tpv1 v2)) _    = Assign id (ICmp op tpv1 v2)
liftNT (Result id (LLVM.FCmp op tpv1 v2)) _    = Assign id (FCmp op tpv1 v2)
liftNT (Result id (LLVM.Phi tp vil)) fn =
  Assign id (Phi tp [ (v, fn p) | (v,p) <- vil ])
liftNT (Result id (LLVM.GEP tp tpvl)) _        = Assign id (GEP tp tpvl)
liftNT (Result id (LLVM.Select tpc tpv1 v2)) _ = Assign id (Select tpc tpv1 v2)
liftNT s _ = liftError $ text "Unsupported nonterminal instruction: " <+> LLVM.ppStmt s

liftBB :: LLVMTranslationInfo -- ^ Translation information from analysis
       -> SymBlockFn
       -> LLVM.BasicBlock
       -> BlockGenerator ()
liftBB lti blockFn bb = do
    id <- case LLVM.bbLabel bb of
            Just id -> return (blockFn id)
            Nothing -> newBlockID
    impl (LLVM.bbStmts bb) id []
  where -- | Sequentially process statements.
        impl :: [LLVM.Stmt] -- ^ Remaining statements
             -> SymBlockID -- ^ Current block that we are defining.
             -> [SymStmt] -- ^ Statements for previous nonterminals in reverse order.
             -> BlockGenerator ()
        impl [] _ _ = liftError $ text "Missing terminal instruction."
        impl [Effect (LLVM.Ret tpv)] id il = 
          defineBlock id (reverse il ++ [MergeReturnAndClear tpv])
        impl [Effect LLVM.RetVoid] id il =
          defineBlock id (reverse il ++ [MergeReturnVoidAndClear])
        impl (Result reg (LLVM.Call _b tp v tpvl):r) id il = do
          -- Allocate block for next block after call.
          nid <- newBlockID
          -- Define previous block to end with pushing call frame.
          defineBlock id (reverse il ++ [PushCallFrame nid (Just reg) tp v tpvl])
          -- Process rest of instructions.
          impl r nid []
        impl (Effect (LLVM.Call _b tp v tpvl):r) id il = do
          -- Allocate block for next block after call.
          nid <- newBlockID
          -- Define previous block to end with pushing call frame.
          defineBlock id (reverse il ++ [PushCallFrame nid Nothing tp v tpvl])
          -- Process rest of instructions.
          impl r nid []
        impl [Effect (LLVM.Jump _tgt)] _id _il = do
          undefined
        impl [Effect (LLVM.Br tpv _b1 _b2)] _id _il = do
          undefined
        impl [Effect LLVM.Unreachable] _id _il = do
          undefined
        impl [Effect LLVM.Unwind] _id _il = do
          undefined
        impl (s:r) id il
          | LLVM.isComment (LLVM.stmtInstr s)
          = impl r id il
          | not (LLVM.isTerminator (LLVM.stmtInstr s)) 
          = impl r id (liftNT s blockFn : il)
          | null r
          = liftError $ text "Unsupported terminal instruction: " <+> LLVM.ppStmt s
          | otherwise
          = liftError $
              text "Terminal instruction found before last instruction: " <+> LLVM.ppStmt s

liftDefine :: LLVMTranslationInfo
           -> LLVM.Define
           -> SymDefine
liftDefine _lti _d = undefined
