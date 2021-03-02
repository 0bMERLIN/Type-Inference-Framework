module TypeInference.Exp
    ( -- * Data Types
      Exp(..)
    , Lit(..)
    ) where

import TypeInference.Types

data Exp
    = Abs String Exp
    | App Exp Exp
    | Var String
    | Lit Lit
    | Let String Exp Exp
    | Fix Exp
    | ADTdef String ADTalternatives Exp

type ADTalternatives = [(String, Type)]

data Lit
    = LitNumber Double

-- data T = C Undefined