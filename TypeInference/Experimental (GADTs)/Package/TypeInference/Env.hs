module TypeInference.Env
    ( -- * Data Types
      Env(..)
      -- * Types
    , GADT_Env
      -- * Functions
    , extend
    , extendGadtEnv
    , remove
    ) where

import Data.Map (Map, insert)

import qualified Data.Map as Map

import TypeInference.InferMonad

data Env p = Env [String] (Map String p)

type GADT_Env k = Map String k

instance Show p => Show (Env p) where
    show (Env _ e) = show e

extend :: InferM (String, p) () (Env p, GADT_Env k)
extend (name, t) = do
    (Env supp e, gadtEnv) <- get
    put $ (Env supp $ insert name t e, gadtEnv)
    pure ()

extendGadtEnv :: InferM (String, k) () (Env p, GADT_Env k)
extendGadtEnv (name, k) = do
    (e, gadtEnv) <- get
    put $ (e, insert name k gadtEnv)
    pure ()

remove :: InferM String () (Env p, GADT_Env k)
remove var = do
    (Env supp e, gadtEnv) <- get
    put $ (Env supp $ Map.delete var e, gadtEnv)
    pure ()