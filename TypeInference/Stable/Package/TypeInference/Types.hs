{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeInference.Types
    ( -- * Data Types
      Type(..)
    , Poly(..)
      -- * Type Classes
    , FreeTVs(..)
      -- * Type Aliases
    , TypeEnv
      -- * Functions
    , getRetT
    ) where

import TypeInference.Env

type TypeVar = String

data Poly
    = Poly [TypeVar] Type
    | Mono Type

data Type
    = Arrow Type Type
    | TypeVar TypeVar
    | Const String
    deriving Eq

type TypeEnv = Env Poly

getRetT (Arrow _ t) = t

class FreeTVs t where
    frees :: t -> [TypeVar]

instance Show Type where
    show = \case
        Arrow t t' -> "(" ++ show t ++ " -> " ++ show t' ++ ")"
        TypeVar s  -> s
        Const   s  -> s

instance Show Poly where
    show = \case
        Mono t -> show t
        Poly qs t -> "forall "
            ++ foldr1 (\a b -> a ++ " " ++ b) qs
            ++ " . "
            ++ show t