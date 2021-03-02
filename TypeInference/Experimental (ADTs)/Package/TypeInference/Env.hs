module TypeInference.Env
    ( -- * Data Types
      Env(..)
    , InferEnv(..)
      -- * Types
    , NoSuppEnv
      -- * Functions
    , extend
    , extendDataTEnv
    , remove
    ) where

import Data.Map (Map, insert)

import qualified Data.Map as Map

import TypeInference.InferMonad
import TypeInference.Types

data Env p = Env [String] (Map String p)

type NoSuppEnv p = Map String p

data InferEnv = InferEnv (Env Poly) (NoSuppEnv Kind)

type TypeEnv = Env Poly

extendDataTEnv :: InferM (String, Kind) () InferEnv
extendDataTEnv (name, k) = do
    (InferEnv e dataTEnv) <- get
    put $ InferEnv e $ insert name k dataTEnv
    pure ()

extend :: InferM (String, Poly) () InferEnv
extend (name, t) = do
    (InferEnv (Env supp e) dataTEnv) <- get
    put $ InferEnv (Env supp $ insert name t e) dataTEnv
    pure ()

remove :: InferM String () InferEnv
remove var = do
    (InferEnv (Env supp e) dataTEnv) <- get
    put $ InferEnv (Env supp $ Map.delete var e) dataTEnv
    pure ()