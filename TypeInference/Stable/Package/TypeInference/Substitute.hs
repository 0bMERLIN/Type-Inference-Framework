{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInference.Substitute
    ( -- * Type Aliases
      Subst(..)
      -- * Type Classes
    , Substitutable
      -- * Functions
    , emptySub
    , compose
    , apply
    , subFromList
    , subLookup
    ) where

import Data.Map   (Map(..))
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

import TypeInference.Types

newtype Subst = Subst (Map String Type)

emptySub                         = Subst $ Map.empty

subFromList                      = Subst . Map.fromList

subLookup name (Subst s)         = Map.lookup name s

compose ss@(Subst s1) (Subst s2) =
    Subst $ Map.map
    (apply ss) s2
    `Map.union` s1

class Substitutable t where
    apply :: Subst -> t -> t

instance Substitutable Type where 
    apply sub = \case
        Arrow fr to  -> Arrow
            (apply sub fr)
            (apply sub to)
        tv@(TypeVar name) -> fromMaybe tv
            $ subLookup name sub
        t -> t

instance Substitutable Poly where
    apply sub@(Subst sMap) = \case
        Mono t    -> Mono $ apply sub t
        Poly qs t -> Poly qs $ apply (sub' qs) t
      where
        -- delete all the entries that are in 'qs'
        sub' qs = Subst $ foldr Map.delete sMap qs

instance Show Subst where
    show (Subst m) = case Map.toList m of
        [] -> ""
        _  -> foldr1 (\a b -> a ++ "\n" ++ b)
            $ Map.mapWithKey (\k v -> show k
            ++ " = " ++ show v) m