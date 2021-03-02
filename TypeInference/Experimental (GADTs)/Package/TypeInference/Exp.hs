module TypeInference.Exp
    ( -- * Data Types
      Exp(..)
    , Lit(..)
    , ADTdef(..)
      -- * Type Aliases
    , ADTalternatives
    ) where

import TypeInference.Types

data Exp
    = Abs String Exp
    | App Exp Exp
    | Var String
    | Lit Lit
    | Let String Exp Exp
    | Fix Exp
    | EADTdef ADTdef
    deriving Show

data ADTdef = ADTdef
    { _name            :: String
    , _quantifiers     :: [String]
    , _ADTalternatives :: ADTalternatives
    , _nextExp         :: Exp
    } deriving Show

type ADTalternatives = [(String, Type)]

data Lit
    = LitNumber Double
    deriving Show