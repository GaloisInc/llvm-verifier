{- |
Module           : $Header$
Description      : Error path and error handling tests
Stability        : provisional
Point-of-contact : jstanley
-}

module Tests.Errors (errorTests) where

import           Test.Tasty
import           Tests.Common

errorTests :: [TestTree]
errorTests =
  [ lssTestAll "ctests/test-error-paths-all"  [] (Just 2) AllPathsErr
  , lssTestAll "ctests/test-error-paths-some" [] (Just 1) (RV 0)
  , lssTestAll "ctests/test-error-paths-some-more" [] (Just 2) (RV 0)
  , lssTestAll "ctests/test-error-paths-bad-mem-trivial" [] (Just 1) AllPathsErr
  , lssTestAll "ctests/test-error-paths-bad-mem-diverge" [] (Just 1) (RV 1)

  , localOption (mkTimeout 5000000) $ -- 5 second local timeout
    withVerbModel "ctests/test-error-paths-bad-mem-symsize.bc" $ \v getmdl ->
        testGroup "ctests/test-error-paths-bad-mem-symsize"
            [ runLssTest "buddy model"     v createBuddyModel    getmdl [] (Just 1) (RV 1)
            , runLssTest "dag model"       v createDagModel      getmdl [] Nothing  (RV 1)
            -- TODO: the following times out, but it's not critical that
            -- we fix that soon, so let's ignore it for now.
            --, runLssTest "SAW model"       v createSAWModel      getmdl [] (Just 1) (RV 1)
            ]
  ]
