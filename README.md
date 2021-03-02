# Type-Inference-Framework

Inline-style: 
![alt text](https://images.squarespace-cdn.com/content/v1/5b69caee7e3c3a59a8f8c363/1548794114201-STHD7QO4SA85AS17UAUN/ke17ZwdGBToddI8pDm48kLkXF2pIyv_F2eUT9F60jBl7gQa3H78H3Y0txjaiv_0fDoOvxcdMmMKkDsyUqMSsMWxHk725yiiHCCLfrh8O1z4YTzHvnKhyp6Da-NYroOW3ZGjoBKy3azqku80C789l0iyqMbMesKd95J-X4EagrgU9L3Sa3U8cogeb0tjXbfawd0urKshkc5MgdBeJmALQKw/iStock-916358708.jpg?format=2500w)

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
