module Expr where

import Types

bop2fn Sum = (+)
bop2fn Mul = (*)
bop2fn Sub = (-)

data Value = VInt Int | VString String

reduce1 :: Context -> E -> Int
reduce1 _ (ELit n) = n
reduce1 c (EVar n) =
  case lookup n c of
    Just (NInt i) -> i
    _ -> error "not a number"
reduce1 c (EBinOp op e1 e2) = r e1 `fn` r e2
  where
    r = reduce1 c
    fn = bop2fn op
reduce1 _ e = error $ "not a number: " ++ show e

reduce2 _ (EString n) = n
reduce2 c (EVar n) =
  case lookup n c of
    Just (NString i) -> i
    _ -> error "not a string"
reduce2 c (EConcat e1 e2) = r e1 ++ r e2
  where
    r = reduce2 c
reduce2 _ e = error $ "not a string: " ++ show e

reduce :: Context -> E -> NodeVar
reduce _ EHole = NHole
reduce _ (EVar n) = NVar n
reduce _ (ENamed n) = NVal (NSymbol n)
reduce _ (EString n) = NVal (NString n)
reduce _ (ELit n) = NVal (NInt n)
reduce c e@(EBinOp{}) = NVal . NInt $ reduce1 c e
reduce c e@(EConcat{}) = NVal . NString $ reduce2 c e
