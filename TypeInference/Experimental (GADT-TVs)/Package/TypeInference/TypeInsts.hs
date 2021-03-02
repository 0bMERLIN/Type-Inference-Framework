{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
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

freshTypeVar :: InferM () Type (TypeEnv, GADT_Env Kind)
freshTypeVar () = do
    (Env supp e, gadtEnv) <- get
    put $ (Env (tail supp) e, gadtEnv)
    pure (TypeVar $ head supp)

substituteEnv :: InferM Subst () (TypeEnv, GADT_Env Kind)
substituteEnv sub = do
    (Env x e, gadtEnv) <- get
    put (Env x $ Map.map (apply sub) e, gadtEnv)
    pure ()

generalize :: InferM Type Poly (TypeEnv, GADT_Env Kind)
generalize t = do
    (e, _) <- get
    let vars = Set.toList
            (Set.fromList
            (frees t)
            `Set.difference`
            Set.fromList
            (frees e))
    case vars of
        []-> pure $ Mono t
        _ -> pure $ Poly vars t

substsFromQuantifiers :: InferM [String] Subst (Env p, GADT_Env k)
substsFromQuantifiers = fmap subFromList
        . (mapM $ \q -> do
        pure (q, TypeVar q))

instantiate :: InferM Poly Type (TypeEnv, GADT_Env Kind)
instantiate = \case
    Mono t    -> pure t
    Poly qs t -> do
        subst <- substsFromQuantifiers qs
        pure $ apply subst t

instance FreeTVs [Type] where
    frees ts =
        let go (t : ts') = (frees t) : (go ts')
            go [] = []
        in case go ts of
            []  -> []
            res -> foldr1 (++) res

instance FreeTVs Type where
    frees = \case
        Arrow t t'  -> nub $ frees t ++ frees t'
        TypeVar tv  -> [tv]
        GADT _ qs   -> frees qs

instance FreeTVs Poly where
    frees = \case
        Mono t -> frees t
        Poly qs t -> Set.toList $
            Set.fromList (frees t)
            `Set.difference`
            Set.fromList qs

instance FreeTVs t => FreeTVs (Env t) where
    frees (Env _ e) =
        let fs = Map.map frees e
        in if Map.null e
        then []
        else foldr1 (++) fs