ghci                                        \
  -i ../Package/TypeInference/Exp.hs        \
     ../Test/Test.hs                        \
     ../Package/TypeInference/AssertInferM.hs\
     ../Package/TypeInference/Env.hs        \
     ../Package/TypeInference/Types.hs      \
     ../Package/TypeInference/TypeInsts.hs  \
     ../Package/TypeInference/Substitute.hs \
     ../Package/TypeInference/Unify.hs      \
     ../Package/TypeInference/Infer.hs      \
     ../Package/TypeInference/InferMonad.hs \
     ../Package/TypeInference.hs            \
     -package 'mtl-2.2.2'                   \
     -package parsec