module TypeInference.Exp
    ( -- * Data Types
      Exp(..)
    , Lit(..)
    ) where

data Exp
    = Abs String Exp
    | App Exp Exp
    | Var String
    | Lit Lit
    | Let String Exp Exp
    | Fix Exp

data Lit
    = LitNumber Double