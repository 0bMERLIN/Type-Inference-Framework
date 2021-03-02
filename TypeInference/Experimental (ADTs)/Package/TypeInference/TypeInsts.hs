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

freshTypeVar :: InferM () Type InferEnv
freshTypeVar () = do
    (InferEnv (Env supp e) dataTEnv) <- get
    put $ InferEnv (Env (tail supp) e) dataTEnv
    pure (TypeVar $ head supp)

substituteEnv :: InferM Subst () InferEnv
substituteEnv sub = do
    (InferEnv (Env x e) dataTEnv) <- get
    put $ InferEnv (Env x $ Map.map (apply sub) e) dataTEnv
    pure ()

generalize :: InferM Type Poly InferEnv
generalize t = do
    (InferEnv e _) <- get
    let vars = Set.toList
            (Set.fromList
            (frees t)
            `Set.difference`
            Set.fromList
            (frees e))
    pure $ Poly vars t

substsFromQuantifiers :: InferM [String] Subst InferEnv
substsFromQuantifiers = fmap subFromList
        . (mapM $ \q -> do
        pure (q, TypeVar q))

instantiate :: InferM Poly Type InferEnv
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