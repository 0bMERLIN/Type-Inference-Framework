module Test
  ( test
  ) where

import TypeInference
import Parser


_T_def ine = EADTdef $ ADTdef "T" ["m", "a"]
  [ ( "mkT", GADT (TypeVar "m") [TypeVar "a"] `Arrow` GADT (Const "T") [TypeVar "a"] )
  ] ine

e = _T_def $ Var "mkT" `App` (Lit $ LitNumber 1)

test = fst $ runInfer e