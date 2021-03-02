module TypeInference
    ( -- * Data Types
      Poly(..)
    , Type(..)
    , Exp (..)
    , Lit (..)
      -- * functions
    , runInfer
    ) where


import TypeInference.Infer
import TypeInference.Unify
import TypeInference.Substitute
import TypeInference.Types
import TypeInference.Exp