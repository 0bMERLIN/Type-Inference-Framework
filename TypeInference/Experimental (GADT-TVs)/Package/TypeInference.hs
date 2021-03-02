module TypeInference
    ( -- * Data Types
      Poly(..)
    , Type(..)
    , Exp (..)
    , ADTdef(..)
    , Lit (..)
      -- * functions
    , runInfer
    , apply
    , unify
    ) where


import TypeInference.Infer
import TypeInference.Unify
import TypeInference.Substitute
import TypeInference.Types
import TypeInference.Exp