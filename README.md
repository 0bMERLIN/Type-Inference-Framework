# Type-Inference-Framework

## Stable:
  the stable version of the type inferencer.
  no support for data structures.



## Experimental (ADTs):
  basically stable + support for ADTs:
 
 ```haskell
  data MaybeInt
  = Just Int
  | Nothing
 ```



## Experimental (GADTs):
  support for adts with type variables:

  ```haskell
  data Maybe a
    = Just a
    | Nothing
  ```

  problems:

  the GADTs can only be concrete. You cant have

  ```haskell
  f :: (a -> m a) -> a -> m a
  ```


## Experimental (GADT-TVs):
  solves the problem of the previous iteration, but
  lacks kind inference, so it sometimes lets errors
  pass.
