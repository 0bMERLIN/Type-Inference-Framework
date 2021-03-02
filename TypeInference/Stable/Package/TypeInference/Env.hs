module TypeInference.Env
    ( -- * Data Types
      Env(..)
      -- * Functions
    , extend
    , remove
    ) where

import Data.Map (Map, insert)

import qualified Data.Map as Map

import TypeInference.InferMonad

data Env p = Env [String] (Map String p)

extend :: InferM (String, p) () (Env p)
extend (name, t) = do
    (Env supp e) <- get
    put $ Env supp (insert name t e)
    pure ()

remove :: InferM String () (Env p)
remove var = do
    (Env supp e) <- get
    put $ Env supp $ Map.delete var e
    pure ()