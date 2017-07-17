--TODO reverse label args
--TODO finish notes, reflect db
--
-- Internal syntax notes
-- 1. rule syntax:
--  rule r n
--    imperative r
--    logical r
--  pattern q r
--    linear q
--    negated q
--  bin-op b r
--  assert q r
--
--  pattern q _
--    label q l
--    var v q n
--    literal l q n
--    hole q n
--
--  var v p n
--    var-name v str
--
--  bin-op b _
--   left e b
--   right e b
--
--  expr e q n
--
--    operator op e
--      op = '+ '* '-
--      operand e1 e 1, operand e2 e 2  # could have more
--
--     var v e
--
--     hole e
--
--
-- 2. tuple syntax notes:
--
--  fact f
--    label l f
--    node n f rank
--
--  tuple t
--    fact f t
--    tid id t
--
--  proof e
--    fact f e
--    cause p e (mm)
--
--  false e
--    fact f e
--
--  cause p
--    rule r p
--    trigger e p
--    matched e p
--    consumed t p
--  | extern p id

{-# LANGUAGE OverloadedStrings #-}
module Reflection where

import Control.Monad
import Data.Maybe
import qualified Data.Set as S (toList)

import Types
import Monad
import Graph
import Index

-- TODO update
actions :: DB -> Index -> Label -> [RawTuple]
actions db ind label = concatMap step triggers
  where
    triggers = indLookup (label, Positive) ind
    step  (_, _, Query _ (EP _ _ rel vs), pattern) = actions
      where
        bindings = solveSteps (tuples db) (facts db) emptyMatchBindings (S.toList pattern)
        -- TODO a rule with a free variable in a user-action should cause an error
        -- it will fail at this fromJust
        bind (ctxt, _, _) = (rel, map (fromJust . flip matchLookup ctxt) vs)
        actions = map bind bindings
    step _ = error "impossible; no QBinOp in Index"

attributes :: Node -> DB -> [Tuple]
attributes n db = filter ok . fromGraph $ tuples db
  where
    ok t = n `elem` nodes t

-- ~~~~~~~~~~ --
-- Reflection --
-- ~~~~~~~~~~ --

makeT :: Label -> [Node] -> M2 ()
makeT (L f) p = do
  t <- packTuple (LA f (length p), p) nullProv
  scheduleAdd t
makeT f p = do
  t <- packTuple (f, p) nullProv
  scheduleAdd t

flattenNV q c n (NVal node) = do
  makeT "literal" [node, q, NInt n]
  makeT ntag [node]
  return c
  where
    ntag =
      case node of
        NInt _ -> "int"
        NNode _ -> "node"
        NSymbol _ -> "symbol"
        NString _ -> "string"
flattenNV q ctxt n (NVar name) =
  case lookup name ctxt of
    Just b -> do
      makeT "var" [b, q, NInt n]
      return ctxt
    Nothing -> do
      b <- freshNode
      makeT "var-name" [b, NString name]
      makeT "var" [b, q, NInt n]
      return $ (name, b) : ctxt

flattenNV q c n (NHole) = makeT "hole" [q, NInt n] >> return c

labelString = NString . lstring

flattenEP q c (EP lin _ l ns) = do
  makeT "label" [q, labelString l]
  unless (lin == NonLinear) (makeT "linear" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenEP q c (LP pol l ns) = do
  makeT "label" [q, labelString l]
  unless (pol == Positive) (makeT "negated" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenQ r c (Query dot ep) = do
  q <- freshNode
  makeT "pattern" [q, r]
  flattenEP q c ep

-- TODO implement
flattenQ r c (QBinOp op e1 e2) = return c

-- TODO finish implementation
flattenE :: Node -> Context -> Int -> E -> M2 Context
flattenE q c n (ELit i) = flattenNV q c n (NVal (NInt i))
flattenE q c n (EString str) = flattenNV q c n (NVal (NString str))
flattenE q c n (ENamed str) = flattenNV q c n (NVal (NSymbol str))
flattenE q c n (EVar str) = flattenNV q c n (NVar str)
flattenE q c n (EHole) = flattenNV q c n NHole
flattenE q c n (EBinOp op e1 e2) = do
    e <- freshNode
    makeT "expr" [e, q, NInt n]
    makeT "operator" [opNode, e]
    c1 <- flattenE e c 1 e1
    c2 <- flattenE e c1 2 e2
    return c2
  where
    opNode =
      case op of
        Sum -> NSymbol "+"
        Sub -> NSymbol "-"
        Mul -> NSymbol "*"
flattenE q c n (EConcat e1 e2) = do
    e <- freshNode
    makeT "expr" [e, q, NInt n]
    makeT "operator" [NSymbol "++", e]
    c1 <- flattenE e c 1 e1
    c2 <- flattenE e c1 2 e2
    return c2

flattenA r c (Assert l es) = do
  q <- freshNode
  makeT "assert" [q, r]
  makeT "label" [q, labelString l]
  foldM (\c -> uncurry $ flattenE q c) c (zip [1..] es)

flattenRule :: Int -> Rule -> M2 ()
flattenRule i rule = do
    r <- freshNode
    makeT "rule" [r, NInt i]
    c <- foldM (flattenQ r) [] qs
    _ <- foldM (flattenA r) c as
    case rule of
      Rule _ _ -> makeT "imperative" [r]
      LRule _ _ -> makeT "logical" [r]
  where
    qs = lhs rule
    as = rhs rule

flattenRules :: [Rule] -> M2 ()
flattenRules rules = mapM_ (uncurry flattenRule) (zip [1..] rules)
