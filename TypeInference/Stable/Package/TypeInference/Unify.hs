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

-- binds a name to a type in a substitution,
-- while also applying an occurs check.
-- tVarBind lives in the InferM monad.
tVarBind (name, t)
    | t == TypeVar name   = pure emptySub
    | elem name (frees t) = throwE $ printf
        "occurs check failed for type %s ~ %s"
        name (show t)
    | otherwise           = pure $ subFromList [(name, t)]

-- unifys type isnt important, so i'll let haskells constraint solver
-- do the work. Just know, that unify lives in the InferM monad.
unify (Const c, Const c') | c == c' = pure emptySub

unify (Arrow fr to, Arrow fr' to') = do
        frSub <- unify (fr, fr')
        toSub <- unify (apply frSub to, apply frSub to')
        pure $ compose frSub toSub

unify (TypeVar tv, TypeVar tv') = tVarBind (tv, TypeVar tv')

unify (TypeVar tv, t)           = tVarBind (tv, t)

unify (t, TypeVar tv)           = tVarBind (tv, t)

unify (t, t')                   = throwE $ printf
        "error: cannot unify %s with %s"
        (show t)
        (show t')