module Test
  ( test
  ) where

import TypeInference

e =
  ADTdef "NumID"
    [("mkNumID", Const "Number" `Arrow` Const "NumID")] $
  ADTdef "NumIDCont"
    [("mkNumIDCont", Const "NumID" `Arrow` Const "NumIDCont")] $
    App (Var "mkNumIDCont")
  $ App (Var "mkNumID") (Lit $ LitNumber 1)

test = runInfer e