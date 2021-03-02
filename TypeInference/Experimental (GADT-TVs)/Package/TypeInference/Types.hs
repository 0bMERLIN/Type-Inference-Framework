{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeInference.Types
    ( -- * Data Types
      Type(..)
    , Poly(..)
    , Kind(..)
      -- * Type Classes
    , FreeTVs(..)
      -- * Type Aliases
    , TypeEnv
    , TypeVar
      -- * Functions
    ) where

import TypeInference.Env

type TypeVar = String

-- Kind 2 = (*, *) -> *
-- Kind 1 = * -> *
-- Kind 0 = *
data Kind = Kind Int deriving Eq

data Poly
    = Poly [TypeVar] Type
    | Mono Type

data Type
    = Arrow { getArgT :: Type, getRetT :: Type }
    | TypeVar TypeVar
    | GADT { getGADT_outer :: Type, getGADT_inners :: [Type]}
    | Const String
    deriving Eq

type TypeEnv = Env Poly

class FreeTVs t where
    frees :: t -> [TypeVar]

instance Show Type where
    show = \case
        Arrow t t' -> "(" ++ show t ++ " -> " ++ show t' ++ ")"
        TypeVar s  -> s
        GADT t []  -> show t
        GADT t qs  -> show t ++ " " ++ (strConcat $ map show qs)
        Const s -> s

strConcatWith :: String -> [String] -> String
strConcatWith _ [] = ""
strConcatWith s xs = foldr1 (\a b -> a ++ s ++ b) xs

strConcat :: [String] -> String
strConcat = strConcatWith " "

instance Show Poly where
    show = \case
        Mono t -> show t
        Poly qs t -> "forall "
            ++ strConcat qs
            ++ " . "
            ++ show t

instance Show Kind where
    show (Kind n)
      = case  map (const "*") [0..n - 1] of
          [] -> "*"
          [_] -> "* -> *"
          stars -> "(" ++ strConcatWith ", " stars ++ ") -> *"