-- Internal syntax notes --
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
--    label l q
--    var v q n
--    literal l q n
--    hole q n
--
--  var v p n
--    var-name v str
--
--  constraint c _
--    operator op c
--      op = '+ '* '-
--      arguments are numbered children
--
--  expr e q n
--    operator op e
--      op = '+ '* '-
--      arguments are numbered children
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
--    cause p t
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
--
--  these are cached during tuple reflection:
--    Rule
--    Fact
--    Event
--    Tuple
--    Provenance

{-# LANGUAGE OverloadedStrings #-}
module Reflection where

import Control.Monad
import Data.Maybe
import qualified Data.Set as S (toList)
import Data.Map (Map)
import qualified Data.Map as M

import Types
import Monad
import Graph
import Index

import Debug.Trace

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

-- ~~~~~~~~~~~ --
-- Rule Syntax --
-- ~~~~~~~~~~~ --
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
  makeT "label" [labelString l, q]
  unless (lin == NonLinear) (makeT "linear" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenEP q c (LP pol l ns) = do
  makeT "label" [labelString l, q]
  unless (pol == Positive) (makeT "negated" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenQ r c (Query dot ep) = do
  q <- freshNode
  makeT "pattern" [q, r]
  flattenEP q c ep

-- TODO implement
flattenQ r c (QBinOp op e1 e2) = do
    q <- freshNode
    makeT "constraint" [q, r]
    makeT "operator" [opNode, q]
    c1 <- flattenE q c 1 e1
    c2 <- flattenE q c1 2 e2
    return c2
  where
    opNode =
      case op of
        QEq -> NSymbol "="
        QDisEq -> NSymbol "/="
        QLess -> NSymbol "<"
        QMore -> NSymbol ">"
        QLessEq -> NSymbol "<="
        QMoreEq -> NSymbol ">="

-- TODO combine operator cases
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
  makeT "label" [labelString l, q]
  foldM (\c -> uncurry $ flattenE q c) c (zip [1..] es)

flattenRule :: ReflContext -> RankedRule -> M2 (Node, ReflContext)
flattenRule rc r | Just i <- M.lookup r (rcr rc) = return (i, rc)
flattenRule rc rr@(i, rule) = do
    r <- freshNode
    makeT "rule" [r]
    makeT "rank" [NInt i, r]
    c <- foldM (flattenQ r) [] qs
    _ <- foldM (flattenA r) c as
    case rule of
      Rule _ _ -> makeT "imperative" [r]
      LRule _ _ -> makeT "logical" [r]
    return (r, rc { rcr = M.insert rr r $ rcr rc })
  where
    qs = lhs rule
    as = rhs rule

flattenRules :: [Rule] -> M2 ()
flattenRules rules = mapM_ (flattenRule emptyRC) (zip [1..] rules)

-- ~~~~~~~~~~~~ --
-- Tuple Syntax --
-- ~~~~~~~~~~~~ --

-- Should use StateT
data ReflContext = RC
  { rcf :: Map Fact Node
  , rcp :: Map Provenance Node
  , rcr :: Map RankedRule Node
  , rce :: Map Event Node
  , rct :: Map Tuple Node
  }

emptyRC = RC { rcf = M.empty
             , rcp = M.empty
             , rcr = M.empty
             , rce = M.empty
             , rct = M.empty
             }

flattenTuple :: ReflContext -> Tuple -> M2 (Node, ReflContext)
flattenTuple rc t | Just i <- M.lookup t (rct rc) = return (i, rc)
flattenTuple rc1 t = do
  i <- freshNode
  makeT "tuple" [i]
  (f, rc2) <- flattenFact rc1 (label t, nodes t)
  (p, rc3) <- flattenProv rc2 (source t)
  makeT "fact" [f, i]
  makeT "cause" [p, i]
  makeT "tid" [NInt (tid t), i]
  let rc4 = rc3 { rct = M.insert t i $ rct rc3 }
  return (i, rc4)

flattenFact rc f | Just i <- M.lookup f (rcf rc) = return (i, rc)
flattenFact rc1 f@(l,ns) = do
  i <- freshNode
  makeT "fact" [i]
  mapM (\(r,n) -> makeT "node" [n, i, NInt r]) (zip [1..] ns)
  makeT "label" [labelString l, i]
  let rc2 = rc1 { rcf = M.insert f i $ rcf rc1 }
  return (i, rc2)

flattenProv rc p | Just i <- M.lookup p (rcp rc) = return (i, rc)
flattenProv rc1 (Extern ids) = do
  i <- freshNode
  makeT "extern" [i]
  let fix (n,num) = makeT "id" [NInt num, i, NInt n]
  mapM_ fix $ zip [1..] ids
  return (i, rc1)
flattenProv rc1 p = do
  i <- freshNode
  makeT "cause" [i]
  (r, rc2) <- flattenRule rc1 (rule_src p)
  makeT "rule" [r, i]
  rc3 <- case tuple_src p of
          Just e -> do
            (ei, temp) <- flattenEvent rc2 e
            makeT "trigger" [ei, i]
            return temp
          Nothing -> return rc2

  let doT c t = do
        (ti, c') <- flattenTuple c t
        makeT "consumed" [ti, i]
        return c'
  let doE c e = do
        (ei, c') <- flattenEvent c e
        makeT "matched" [ei, i]
        return c'

  rc4 <- foldM doT rc3 (consumed p)
  rc5 <- foldM doE rc4 (matched p)

  return (i, rc5)

flattenEvent rc e | Just i <- M.lookup e (rce rc) = return (i, rc)
flattenEvent rc1 (E pol t) = do
  i <- freshNode
  (ti, rc2) <- flattenTuple rc1 t
  makeT (pol' pol) [i]
  makeT "tuple" [ti, i]
  return (i, rc2)
  where
    pol' Positive = "positive"
    pol' Negative = "negative"
flattenEvent rc1 (EFact f ps) = do
  i <- freshNode
  (fi, rc2) <- flattenFact rc1 f
  makeT "proof" [i]
  makeT "fact" [fi, i]
  let fix c p = do
        (pi, c') <- flattenProv c p
        makeT "cause" [pi, i]
        return c'
  rc3 <- foldM fix rc2 ps
  return (i, rc3)
flattenEvent rc1 (EFalse f) = do
  i <- freshNode
  (fi, rc2) <- flattenFact rc1 f
  makeT "false" [i]
  makeT "fact" [fi, i]
  return (i, rc2)
