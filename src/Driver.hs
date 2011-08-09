import Control.Monad.Trans
import LSS.Execution.Common
import LSS.Execution.Utils
--import LSS.SBEConcrete
import LSS.SBEInterface
import LSS.SBESymbolic
import LSS.Simulator
import Text.LLVM
import Verinf.Symbolic.Common (PrettyTerm(..))

main :: IO ()
main = do
  -- <repo_root>/LLVM/src/testModule.bc for now
  cb <- loadCodebase "testModule.bc"

  let i32 = iT 32

  runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
    i1 <- withSBE $ \sbe -> termInt sbe 32 2
    i2 <- withSBE $ \sbe -> termInt sbe 32 3
    callDefine (Symbol "int32_add") i32
      [ i32 =: IValue 32 i1 , i32 =: IValue 32 i2 ]
    mrv <- getProgramReturnValue
    dbugM $ "Driver: program return value is: " ++ maybe ("none") prettyTerm mrv

  runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
    i1 <- withSBE $ \sbe -> termInt sbe 32 2
    callDefine (Symbol "int32_square") i32 [ i32 =: IValue 32 i1 ]
    mrv <- getProgramReturnValue
    dbugM $ "Driver: program return value is: " ++ maybe ("none") prettyTerm mrv

  runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
    i1 <- withSBE $ \sbe -> termInt sbe 32 2
    i2 <- withSBE $ \sbe -> termInt sbe 32 3
    callDefine (Symbol "int32_muladd") i32
      [ i32 =: IValue 32 i1 , i32 =: IValue 32 i2 ]
    mrv <- getProgramReturnValue
    dbugM $ "Driver: program return value is: " ++ maybe ("none") prettyTerm mrv

  putStrLn "runSimulator completed."
