import Control.Monad.Trans
import LSS.SBEInterface
import LSS.Execution.Common
import LSS.Simulator
import Text.LLVM

main :: IO ()
main = do
  cb <- loadCodebase "foo.bc"

  runSimulator cb sbeStub liftSBEStub $ do
    callDefine (Symbol "int32_add") (iT 32)
      [ iT 32 =: IValue 32 2, iT 32 =: IValue 32 3 ]

  -- Here to ensure that use of different SBEs typecheck
  runSimulator cb sbeStubTwo liftSBEStubTwo $ return ()

  putStrLn "runSimulator completed."

liftSBEStub :: SBEStub a -> Simulator SBEStub IO a
liftSBEStub = SM . lift . liftStubToIO

liftSBEStubTwo :: SBEStubTwo a -> Simulator SBEStubTwo IO a
liftSBEStubTwo = SM . lift . liftStubTwoToIO
