{-# LANGUAGE LambdaCase #-}

module TypeInference.Infer
    ( runInfer
    ) where

import Text.Printf (printf)

import qualified Data.Map as Map

import TypeInference.Env
import TypeInference.Exp
import TypeInference.InferMonad
import TypeInference.Substitute
import TypeInference.Types
import TypeInference.TypeInsts
import TypeInference.Unify

class TypeInferrable f where
    infer :: InferM f (Subst, Type) InferEnv

instance TypeInferrable Lit where
    infer = \case
        LitNumber _ ->
            pure (emptySub, Const "Number")

instance TypeInferrable Exp where
    infer = \case
        App f a -> do
            tv <- freshTypeVar ()
            (fs, ft) <- infer f
            (as, at) <- infer a
            s <- unify
                ( apply as ft
                , Arrow at tv)
            pure( s `compose`
                 as `compose`
                 fs `compose`
                 fs, apply s tv)
        Abs p b -> do
            ptv <- freshTypeVar ()
            remove p
            extend (p, Mono ptv)
            (bs, bt) <- infer b
            pure (bs, Arrow (apply bs ptv) bt)
        Var s -> do
            (InferEnv (Env _ e) _) <- get
            case Map.lookup s e of
                Nothing -> throwE $ printf
                    "unbound name %s" s
                Just t -> do
                    t' <- instantiate t
                    pure (emptySub, t')
        Lit l -> infer l
        Let name val bdy -> do
            (vs, vt) <- infer val
            remove name
            substituteEnv vs
            vt' <- generalize vt
            extend (name, vt')
            substituteEnv vs
            (bs, bt) <- infer bdy
            pure (compose bs vs, bt)
        Fix e -> do
            (s, t) <- infer e
            -- getRetT is probably going to fuck me over
            -- at some point, but whatever
            pure (s, getRetT $ apply s t)
        ADTdef _ [] e -> infer e

        ADTdef adtName ((name, t) : rest) e -> do
            -- ADTs dont have type params,
            -- so GADTs would probably be easier
            -- in this system
            extendDataTEnv (adtName, Kind 0)
            verifyT t
            extend (name, Mono t)
            infer (ADTdef adtName rest e)

verifyT :: InferM Type () InferEnv
verifyT = \case
    Arrow t t'  -> do
        verifyT t
        verifyT t'
    TypeVar name-> pure ()
    Const name  -> do
        (InferEnv _ e) <- get
        case Map.lookup name e of
            Nothing -> throwE
                ( "error: Undefined Type '"
                ++ name
                ++"'")
            Just _ -> pure ()

typeVarSrc :: [String]
typeVarSrc =
    [ c : (if n > 0 then show n else "")
    | n <- [0..], c <- ['a'..'z']
    ]

stdTypes :: Map.Map String Kind
stdTypes = Map.fromList
    [ ("Number", Kind 0)
    , ("Bool", Kind 0)
    ]

runInfer :: Exp -> Either InferException (Subst, Type)
runInfer e =
    fst $ runState
        (runExceptT go)
        (InferEnv (Env typeVarSrc $ Map.empty) stdTypes)
  where
      go = do
        (s, t) <- infer e

        pure (s, apply s t)