module TypeInference.InferMonad
    ( -- * Type Aliases
      InferM
    , InferException
      -- * Data Types
    , State
    , ExceptT
      -- * Functions
    , runState
    , runExceptT
    , throwE
    , get
    , put
    ) where

import Control.Monad.State
    ( State
    , runState
    , get
    , put)

import Control.Monad.Trans.Except
    ( ExceptT
    , runExceptT
    , throwE)

type InferException = String

type InferM from to state
    =  from
    -> ExceptT InferException (State state) to