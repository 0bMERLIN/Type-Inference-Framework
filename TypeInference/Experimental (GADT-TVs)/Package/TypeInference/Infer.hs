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
import TypeInference.Assertion

class TypeInferrable f where
    infer :: InferM f (Subst, Type) (TypeEnv, GADT_Env Kind)

instance TypeInferrable Lit where
    infer = \case
        LitNumber _ ->
            pure (emptySub, GADT (Const "Number") [])

instance TypeInferrable Exp where
    infer = \case
        App f a -> do
            (fs, ft) <- infer f
            (as, at) <- infer a
            tv <- freshTypeVar ()
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
            (Env _ e, _) <- get
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
            pure (s, getRetT $ apply s t)
        EADTdef adt ->
            infer adt

instance TypeInferrable ADTdef where
    infer (ADTdef adtName qs alts inExp) = do
        extendGadtEnv (adtName, Kind $ length qs)
        validateGADT_alts alts
        (Env src e, gadtEnv) <- get
        put (Env (applyN (length qs) tail $ src) e, gadtEnv)
        mapM (\(name, t) -> extend ( name, Poly qs t)) alts
        infer inExp
      where
        applyN = (foldr (.) id.) . replicate

validateGADT_alts :: InferM ADTalternatives () (TypeEnv, GADT_Env Kind)
validateGADT_alts alts = do
    mapM (\(name, t) -> validateGADT_altType t) alts
    pure ()

validateGADT_altType :: InferM Type () (TypeEnv, GADT_Env Kind)
validateGADT_altType typ = case typ of
    Arrow t t' -> do
        validateGADT_altType t
        validateGADT_altType t'
    TypeVar _ -> pure ()
    GADT t qs -> do
        validateGADT_altType t
        mapM validateGADT_altType qs
        pure ()
    Const s -> do
        (_, e) <- get
        if Map.lookup s e == Nothing
        then throwE $ "unbound type constructor " ++ s
        else pure ()

typeVarSrc :: [String]
typeVarSrc =
    [ c : (if n > 0 then show n else "")
    | n <- [0..], c <- ['a'..'z']
    ]

runInfer :: TypeInferrable e => e ->
    ( Either InferException (Subst, Type)
    , (TypeEnv , GADT_Env Kind) )
runInfer e = runState (runExceptT go) $
  ( Env typeVarSrc $
      Map.fromList
      [ ("add", Mono $ Arrow (GADT (Const "Number") []) (Arrow (GADT (Const "Number") []) (GADT (Const "Number") [])))
      , ("eq", Poly ["'a"] $ Arrow (TypeVar "'a") (Arrow (TypeVar "'a") (GADT (Const "Boolean") [])))
      ]
  , Map.fromList
    [ ("Number", Kind 0)
    , ("Boolean", Kind 0)
    ]
  )
  where
      go = do
        (s, t) <- infer e
        pure (s, apply s t)