import Control.Monad.Trans
import LSS.SBEInterface
import LSS.SBESymbolic
import LSS.SBEConcrete
import LSS.Execution.Common
import LSS.Simulator
import Verinf.Symbolic.Common hiding (termInt)
import Verinf.Symbolic.Lit.ABC
import Text.LLVM

instance Show (LitResult a) where
  show = const "TODO"

main :: IO ()
main = do
  cb <- loadCodebase "foo.bc"

  {-
  runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
    i1 <- withSBE $ \sbe -> termInt sbe 32 2
    i2 <- withSBE $ \sbe -> termInt sbe 32 3
    callDefine (Symbol "int32_add") (iT 32)
      [ iT 32 =: IValue 32 i1 , iT 32 =: IValue 32 i2 ]
  -}

  be <- createAbcBitEngine
  runSimulator cb (sbeSymbolicBit be) (SM . lift . liftSBESymbolicBit) $ do
    i1 <- withSBE $ \sbe -> termInt sbe 32 2
    i2 <- withSBE $ \sbe -> termInt sbe 32 3
    callDefine (Symbol "int32_add") (iT 32)
      [ iT 32 =: IValue 32 i1 , iT 32 =: IValue 32 i2 ]

  -- Here to ensure that use of different SBEs typecheck
  runSimulator cb sbeConcrete (SM . lift . liftSBEConcrete) $ return ()

  putStrLn "runSimulator completed."
