{-# LANGUAGE LambdaCase #-}

module TypeInference.Unify
    ( unify
    ) where

import Text.Printf (printf)

import qualified Data.Map as Map

import TypeInference.Types
import TypeInference.TypeInsts
import TypeInference.Substitute
import TypeInference.InferMonad
import TypeInference.Env

tVarBind :: InferM (String, Type) Subst (TypeEnv, GADT_Env Kind)
tVarBind (name, t)
    | t == TypeVar name   = pure emptySub
    | elem name (frees t) = throwE $ printf
        "occurs check failed for type %s ~ %s"
        name (show t)
    | otherwise           = pure $ subFromList [(name, t)]

unify :: InferM (Type, Type) Subst (TypeEnv, GADT_Env Kind)
unify (Arrow fr to, Arrow fr' to') = do
        frSub <- unify (fr, fr')
        toSub <- unify (apply frSub to, apply frSub to')
        pure $ compose frSub toSub

unify (TypeVar tv, TypeVar tv') = tVarBind (tv, TypeVar tv')

unify (TypeVar tv, t) = tVarBind (tv, t)

unify (t, TypeVar tv) = tVarBind (tv, t)

unify (GADT name qs, GADT name' qs') = do
    ss <- mapM unify $ zip qs qs'
    case ss of
        []    -> pure $ emptySub
        h : t -> pure $ foldr compose h t

unify (t, t') = throwE $ printf
        "error: cannot unify %s with %s"
        (show t)
        (show t')