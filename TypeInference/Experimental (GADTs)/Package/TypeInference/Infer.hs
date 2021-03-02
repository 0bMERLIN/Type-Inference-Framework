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
            pure (emptySub, GADT "Number" [])

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
        (Env src e, gadtEnv) <- get
        put (Env (applyN (length qs) tail $ src) e, gadtEnv )
        mapM (\(name, t) -> verifyT t >> extend ( name, Poly qs t)) alts
        infer inExp
      where
        applyN = (foldr (.) id.) . replicate

verifyT :: InferM Type () (TypeEnv, GADT_Env Kind)
verifyT = \case
    GADT name qs -> do
        -- test if the type is in the environment
        -- and whether the kind matches or not
        (_, e) <- get
        case Map.lookup name e of
            Nothing -> throwE ("error: Undefined Type '"++ name ++ "'")
            Just actualK ->
                let expectedK = Kind $ length qs
                in if expectedK /= actualK
                then throwE ( name
                    ++ " has kind "
                    ++ show actualK
                    ++ ", but is applied as if it were of kind "
                    ++ show expectedK )
                else pure ()
    Arrow t t'  -> do
        verifyT t
        verifyT t'
    TypeVar name-> pure ()

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
      [ ("add", Mono $ Arrow (GADT "Number" []) (Arrow (GADT "Number" []) (GADT "Number" [])))
      , ("eq", Poly ["'a"] $ Arrow (TypeVar "'a") (Arrow (TypeVar "'a") (GADT "Boolean" [])))
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