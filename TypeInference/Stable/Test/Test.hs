module Test
  ( test
  ) where

import TypeInference

eLet name val bdy = App (Abs name bdy) val

e0 = Let "id" (Abs "x" (Var "x")) (Var "id")
e1 = Let "id" (Abs "x" (Var "x")) (App (Var "id") (Var "id"))
e2 = Let "id" (Abs "x" (Let "y" (Var "x") (Var "y"))) (App (Var "id") (Var "id"))
e3 = Let "id" (Abs "x" (Let "y" (Var "x") (Var "y"))) (App (App (Var "id") (Var "id")) (Lit (LitNumber 2)))
e4 = Let "id" (Abs "x" (App (Var "x") (Var "x"))) (Var "id")

e5  = Let "rec"
    (Fix
        (Abs "self"
            (App (Var "self") (Lit $ LitNumber 0))
        )
    )
    ( Var "rec"
    )

test = runInfer e5