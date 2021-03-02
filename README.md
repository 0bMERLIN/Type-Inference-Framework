# Type-Inference-Framework

<!--- ![alt text]() -->

## Stable:
  the stable version of the type inferencer.
  no support for data structures.
### Examples:
  ```haskell
  -- infinitely recursive function
  let recursive = fix \self -> \x -> self x
  in recursive 20
  ```
  returns
  ```haskell
  b -> c
  ```
  basically the same as in ghci
  ```haskell
  > let rec x = rec x
  > :t rec
  > rec :: t1 -> t2
  ```



## Experimental (ADTs):
  latest semi-stable version
  
  basically stable + support for ADTs:
 
 ```haskell
  data MaybeInt
    = Just Int
    | Nothing
 ```



## Experimental (GADTs):
  WARNING:
    from this version on, I would not advise you to use the framework.
    It lets very common errors pass, that can result in segfaults later in the compiler pipeline.
  support for adts with type variables:

  ```haskell
  data Maybe a
    = Just a
    | Nothing
  ```

  problems:
  - the GADTs can only be concrete. You cant have

  ```haskell
  f :: (a -> m a) -> a -> m a
  ```


## Experimental (GADT-TVs):
  solves the problem of the previous iteration, but
  lacks kind inference, so it very often lets errors
  pass.

## Planned Experimental (GADT-Kinds)
  Experimental (GADT-TVs) + Kind Inference.
