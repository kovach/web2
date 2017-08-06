-- TODO update; move this into a document.
--
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
--    Tuple
--    Provenance
--  to reduce output size.

{-# LANGUAGE OverloadedStrings #-}
module Reflection where

import Control.Monad
import Control.Monad.State
import qualified Data.Set as S (toList)
import Data.Map (Map)
import qualified Data.Map as M

import Types
import Monad
import Graph

-- TODO update
--actions :: DB -> Index -> Label -> [RawTuple]
--actions db ind label = concatMap step triggers
--  where
--    triggers = indLookup (label, Just (Truth True)) ind
--    step  (_, _, Query _ (EP _ rel vs), pattern) = actions
--      where
--        bindings = solveSteps (tuples db) emptyMatchBindings (S.toList pattern)
--        -- TODO a rule with a free variable in a user-action should cause an error
--        -- it will fail at this fromJust
--        bind (ctxt, _, _) = (rel, map (fromJust . flip matchLookup ctxt) vs)
--        actions = map bind bindings
--    step _ = error "impossible; no QBinOp in Index"
--
--attributes :: Node -> DB -> [Tuple]
--attributes n db = filter ok . fromGraph $ tuples db
--  where
--    ok t = n `elem` nodes t

type M3 = StateT ReflContext M2

freshProv = do
  NNode n <- freshNode
  return $ Extern [n]

makeT :: Label -> [Node] -> M3 ()
makeT (L f) p = lift $ do
  pr <- freshProv
  t <- packTuple (LA f (length p), p) pr
  scheduleAdd t
makeT f p = lift $ do
  pr <- freshProv
  t <- packTuple (f, p) pr
  scheduleAdd t

fresh :: M3 Node
fresh = lift freshNode

type Lens a b = (a -> b, (b -> b) -> a -> a)
type RCLens f = Lens ReflContext (Map f Node)
flens :: RCLens Fact
flens = (rcf, \f rc -> rc { rcf = f (rcf rc) })
plens :: RCLens Provenance
plens = (rcp, \f rc -> rc { rcp = f (rcp rc) })
rlens :: RCLens RankedRule
rlens = (rcr, \f rc -> rc { rcr = f (rcr rc) })
tlens :: RCLens Tuple
tlens = (rct, \f rc -> rc { rct = f (rct rc) })

withL :: Ord v => RCLens v -> v -> M3 Node -> M3 Node
withL (get, set) t m = do
  mp <- gets get
  case M.lookup t mp of
    Just i -> return i
    Nothing -> do
      i <- m
      modify $ set (M.insert t i)
      return i

withF = withL (flens)
withP = withL (plens)
withR = withL (rlens)
withT = withL (tlens)

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
      b <- fresh
      makeT "var-name" [NString name, b]
      makeT "var" [b, q, NInt n]
      return $ (name, b) : ctxt

flattenNV q c n (NHole) = makeT "hole" [q, NInt n] >> return c

labelString = NString . lstring

flattenEP q c (EP lin l ns) = do
  makeT "label" [labelString l, q]
  unless (lin == NonLinear) (makeT "linear" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenEP q c (LP pol l ns) = do
  makeT "label" [labelString l, q]
  unless (pol == Positive) (makeT "negated" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenQ r c (Query dot ep) = do
  q <- fresh
  makeT "pattern" [q, r]
  flattenEP q c ep

flattenQ r c (QBinOp op e1 e2) = do
    q <- fresh
    makeT "constraint" [q, r]
    makeT "operator" [opNode, q]
    c1 <- flattenE q c 1 e1
    c2 <- flattenE q c1 3 e2
    return c2
  where
    opNode =
      case op of
        QEq -> NString "="
        QDisEq -> NString "/="
        QLess -> NString "<"
        QMore -> NString ">"
        QLessEq -> NString "<="
        QMoreEq -> NString ">="

-- TODO combine operator cases
flattenE :: Node -> Context -> Int -> E -> M3 Context
flattenE q c n (ELit i) = flattenNV q c n (NVal (NInt i))
flattenE q c n (EString str) = flattenNV q c n (NVal (NString str))
flattenE q c n (ENamed str) = flattenNV q c n (NVal (NSymbol str))
flattenE q c n (EVar str) = flattenNV q c n (NVar str)
flattenE q c n (EHole) = flattenNV q c n NHole
flattenE q c n (EBinOp op e1 e2) = do
    e <- fresh
    makeT "expr" [e, q, NInt n]
    makeT "operator" [opNode, e]
    c1 <- flattenE e c 1 e1
    c2 <- flattenE e c1 2 e2
    return c2
  where
    opNode =
      case op of
        Sum -> NString "+"
        Sub -> NString "-"
        Mul -> NString "*"
flattenE q c n (EConcat e1 e2) = do
    e <- fresh
    makeT "expr" [e, q, NInt n]
    makeT "operator" [NString "++", e]
    c1 <- flattenE e c 1 e1
    c2 <- flattenE e c1 2 e2
    return c2

flattenA r c (Assert l es) = do
  q <- fresh
  makeT "assert" [q, r]
  makeT "label" [labelString l, q]
  foldM (\c -> uncurry $ flattenE q c) c (zip [1..] es)

flattenRule :: Maybe Node -> RankedRule -> M3 Node
flattenRule rs rr@(RankedRule i rule) = withR rr $ do
    r <- fresh
    makeT "rule" [r]
    case rule_id rule of
      Just id -> makeT "rule-id" [id, r]
      Nothing -> makeT "extern" [r]
    makeT "rank" [NInt i, r]
    c <- foldM (flattenQ r) [] qs
    _ <- foldM (flattenA r) c as
    case rtype rule of
      Event -> makeT "imperative" [r]
      View -> makeT "logical" [r]
    return r
  where
    qs = lhs rule
    as = rhs rule

flattenRules :: [RankedRule] -> M3 Node
flattenRules rules = do
  i <- fresh
  makeT "rule-set" [i]
  mapM_ (flattenRule $ Just i) rules
  return i

-- ~~~~~~~~~~~~ --
-- Tuple Syntax --
-- ~~~~~~~~~~~~ --
flattenTuple :: Tuple -> M3 Node
flattenTuple t = withT t $ do
  i <- fresh
  makeT "tuple" [i]
  f <- flattenFact (label t, nodes t)
  p <- flattenProv (source t)
  makeT "fact" [f, i]
  makeT "cause" [p, i]
  makeT "tid" [NInt (tid t), i]
  case tval t of
    Truth b -> if b then makeT "true" [i] else makeT "false" [i]
    NoVal -> return ()
  return i

flattenFact f@(l,ns) = withF f $ do
  i <- fresh
  makeT "fact" [i]
  mapM (\(r,n) -> makeT "node" [n, i, NInt r]) (zip [1..] ns)
  makeT "label" [labelString l, i]
  return i

flattenProv p@(Extern ids) = withP p $ do
  i <- fresh
  makeT "extern" [i]
  let fix (n,num) = makeT "id" [NInt num, i, NInt n]
  mapM_ fix $ zip [1..] ids
  return i
flattenProv p@(Provenance{}) = withP p $ do
  i <- fresh
  makeT "cause" [i]
  makeT "event" [i]
  r <- flattenRule Nothing (rule_src p)
  makeT "rule" [r, i]
  case tuple_src p of
    Just e -> do
      ei <- flattenTuple e
      makeT "trigger" [ei, i]
    Nothing -> return ()
  let doT t = do
        ti <- flattenTuple t
        makeT "consumed" [ti, i]
  let doE e = do
        ei <- flattenTuple e
        makeT "matched" [ei, i]
  mapM doT (consumed p)
  mapM doE (matched p)
  return i
flattenProv p@(Reduction{}) = withP p $ do
  i <- fresh
  makeT "cause" [i]
  makeT "reduced" [i]
  let ops = case reduction_op p of
        Or -> "or"
  makeT "op" [NString ops, i]
  let doT t = do
        ti <- flattenTuple t
        makeT "matched" [ti, i]
  mapM doT (reduced p)
  return i

flattenContext :: Context -> M3 Node
flattenContext ctxt = do
  c <- fresh
  mapM_ (fix c) ctxt
  return c
  where
    fix c (name, node) = do
      i <- fresh
      makeT "var-name" [NString name, i]
      makeT "value" [node, i]
      makeT "binding" [i, c]

runReflection :: M3 a -> ReflContext -> M2 (a, ReflContext)
runReflection = runStateT
