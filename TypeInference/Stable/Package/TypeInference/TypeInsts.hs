{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInference.TypeInsts
    ( -- * Functions
      instantiate
    , generalize
    , freshTypeVar
    , substituteEnv
    ) where

import Data.List (nub)

import qualified Data.Map as Map
import qualified Data.Set as Set

import TypeInference.Types
import TypeInference.Substitute
import TypeInference.Env
import TypeInference.InferMonad

freshTypeVar :: InferM () Type TypeEnv
freshTypeVar () = do
    (Env supp e) <- get
    put $ Env (tail supp) e
    pure (TypeVar $ head supp)

substituteEnv :: InferM Subst () TypeEnv
substituteEnv sub = do
    (Env x e) <- get
    put $ Env x $ Map.map (apply sub) e
    pure ()

generalize :: InferM Type Poly TypeEnv
generalize t = do
    e <- get
    let vars = Set.toList
            (Set.fromList
            (frees t)
            `Set.difference`
            Set.fromList
            (frees e))
    pure $ Poly vars t

substsFromQuantifiers :: InferM [String] Subst TypeEnv
substsFromQuantifiers = fmap subFromList
        . (mapM $ \q -> do
        pure (q, TypeVar q))

instantiate :: InferM Poly Type TypeEnv
instantiate = \case
    Mono t    -> pure t
    Poly qs t -> do
        subst <- substsFromQuantifiers qs
        pure $ apply subst t

instance FreeTVs Type where
    frees = \case
        Arrow t t' -> nub $ frees t ++ frees t'
        TypeVar tv -> [tv]
        Const _    -> []

instance FreeTVs Poly where
    frees = \case
        Mono t -> frees t
        Poly qs t -> Set.toList $
            Set.fromList (frees t) `Set.difference` Set.fromList qs

instance FreeTVs t => FreeTVs (Env t) where
    frees (Env _ e) = foldr1 (++) $ Map.map frees e