module Expr where

import Data.String
import Types

bop2fn Sum = (+)
bop2fn Mul = (*)
bop2fn Sub = (-)

reduce1 :: Context -> E -> Int
reduce1 _ (ELit n) = n
reduce1 c (EVar n) =
  case lookup n c of
    Just (NTInt i) -> i
    _ -> error "not a number"
reduce1 c (EBinOp op e1 e2) = r e1 `fn` r e2
  where
    r = reduce1 c
    fn = bop2fn op

reduce :: Context -> E -> NodeVar
reduce _ EHole = NHole
reduce _ (EVar n) = NVar n
reduce _ (ENamed n) = NVal (NTNamed n)
reduce c e = NVal . NTInt $ reduce1 c e
