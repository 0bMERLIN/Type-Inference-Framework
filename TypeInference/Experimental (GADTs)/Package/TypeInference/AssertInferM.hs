module TypeInference.Assertion
    ( assertInfer
    ) where

import TypeInference.InferMonad
import TypeInference.Env
import TypeInference.Types

assertInfer :: Show a => InferM (a -> Bool, a) () TypeEnv
assertInfer (f, a) = do
    if f a
    then pure ()
    else throwE $ "error: Assertion failed for " ++ show a