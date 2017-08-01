{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Graph where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Types
import Expr
import Monad
import Index

assert :: Bool -> a -> [a]
assert True a = [a]
assert False _ = []

matchLookup :: NodeVar -> Context -> Maybe Node
matchLookup (NVal v) _ = Just v
matchLookup (NVar n) c = lookup n c
matchLookup NHole    c = Nothing

-- looks up value in context; generates fresh node if unbound
applyLookup :: NodeVar -> Context -> M2 (Node, Context)
applyLookup v c | Just v <- matchLookup v c = return (v, c)
applyLookup (NVar n) c = do
  v <- freshNode
  return (v, (n, v) : c)
applyLookup NHole c = do
  v <- freshNode
  return (v, c)
applyLookup (NVal v) c = return (v, c)

-- process all unification instances for a given tuple
edgeMatch :: Label -> Context -> [NodeVar] -> [Node] -> Maybe Context
-- TODO check this statically
edgeMatch l c vs nodes | length nodes /= length vs = error $ "relation/pattern arity mismatch! tuple involved: " ++ unwords (show l : (map show nodes))
edgeMatch _ c vs nodes =
    foldM matchStep c $ zip nodes vs
  where
    -- process one variable unification instance on the LHS
    matchStep :: Context -> (Node, NodeVar) -> Maybe Context
    matchStep c (node, (NVar n)) =
      case lookup n c of
        Nothing -> Just $ (n, node) : c
        Just v -> if v == node then Just c else Nothing
    matchStep c (node, (NVal v)) = if v == node then Just c else Nothing
    matchStep c (node, NHole) = Just c

-- Label passed only for arity error reporting
solvePattern :: Label -> [Event] -> Bindings -> Linear -> [NodeVar] -> [Bindings]
solvePattern l es (ctxt, bound, deps, forced) linear nvs =
    let pairs =
          mapMaybe (\t -> fmap (t,) $ edgeMatch l ctxt nvs (nodes t))
          . filter (not . (`elem` (map toEvent bound)))
          $ es
    in do
      (e, newC) <- pairs
      let newBound = if linear == Linear then e : bound else bound
      return (newC, newBound, e : deps, forced)

solveStep :: Graph -> Bindings -> Query -> [Bindings]
solveStep g b@(c, _, _, _) (Query _ (EP linear e vs@(NVar v : _)))
  | Just l <- lookup v c =
    solvePattern e (map toEvent $ constrainRelation1 (TP1 e 0 l) g) b linear vs
solveStep g b@(c, _, _, _) (Query _ (EP linear e vs)) =
    solvePattern e (map toEvent $ constrainRelation e g) b linear vs

solveStep g b@(c, _, _, _) q@(Query _ (LP Positive e ns@(NVar v : _)))
  | Just l <- lookup v c =
    let es = filter isPositive $ constrainRelation1 (TP1 e 0 l) g
    in solvePattern e es b NonLinear ns
solveStep g b@(c, _, _, _) q@(Query _ (LP Positive e ns@(_:NVar v : _)))
  | Just l <- lookup v c =
    let es = filter isPositive $ constrainRelation1 (TP1 e 1 l) g
    in solvePattern e es b NonLinear ns
solveStep g b@(c, bound, deps, forced) q@(Query _ (LP polarity e ns)) =
  case polarity of
    Positive -> solvePattern e trueEs b NonLinear ns
    Negative ->
      let vs = mapM (\n -> matchLookup n c) ns in
      case vs of
        -- TODO reorder queries; remove this constraint
        Nothing -> error $ "negated queries must refer to bound values, or else be the sole clause of a query. offending clause:\n  " ++ show q
        Just vs' ->
          let fact = (e, vs') in
          case filter ((== fact) . tfact) es of
            -- TODO generalize default value based on type
            -- should be able to use syntax (new field in LP)
            [isTrue] | tval isTrue == Truth True -> []
            [isFalse] -> return (c, bound, isFalse : deps, forced)
            _ -> return (c, bound, deps, (fact, Truth False) : forced)
            ts -> error $ "solveStep. INTERNAL ERROR. duplicate proofs of reduced tuple:\n" ++ unlines (map ppTuple ts)
  where
    es = constrainRelation e g
    trueEs = filter isPositive es

solveStep _ b@(c, _, _, _) (QBinOp op v1 v2) =
    case (matchLookup (reduce c v1) c, matchLookup (reduce c v2) c) of
      (Just v1', Just v2') -> assert (op2fn op v1' v2') b
      _ -> error "(in)equality constraints must refer to bound values"
  where
    -- interpret binary op
    op2fn QEq = (==)
    op2fn QDisEq = (/=)
    op2fn QLess = (<)
    op2fn QLessEq = (<=)
    op2fn QMore = (>)
    op2fn QMoreEq = (>=)

solveSteps :: Graph -> Bindings -> [Query] -> [Bindings]
solveSteps g c es = foldM (solveStep g) c es

-- Main matching function --
getMatches :: Event -> RankedRule -> Graph -> [Match]
getMatches ev rule g = takeValid [] . map toMatch . go $ triggers
  where
    triggers = indLookup (label ev, tpolarity ev) (indexRule $ ranked_rule rule)

    pow [] = [[]]
    pow (x@(Linear, _, _, _):xs) = pow xs ++ [[x]]
    pow (x:xs) = p ++ map (x:) p
      where p = pow xs

    -- tail drops the [] case
    go = concatMap stepn . tail . pow

    pat (_,_,p,_) = p

    stepn [] = [] -- shouldn't happen
    stepn (t@(_,_,p,pattern):ts) = bindings
      where
        -- 1. Partition the query into
        --      p1: some non-empty subset of patterns that may match the current event
        --          (computed already by pow)
        --      p2: the rest
        -- 2. Unify every pattern in p1 against ev alone
        -- 3. Unify p2 against g
        ps = map pat ts
        p1 = p : ps
        p2 = foldr S.delete pattern ps

        b0 = emptyMatchBindings
        bindings = do
          b1 <- foldM (\b (Query _ p) ->
                        solvePattern (label ev) [ev] b (epLinear p) (epNodes p)) b0 p1
          solveSteps g b1 (S.toAscList p2)

    toMatch (ctxt, consumed, matched, forced) =
      (Provenance rule (Just ev) matched consumed, ctxt, forced)

    takeValid :: Consumed -> [Match] -> [Match]
    takeValid _ [] = []
    takeValid removed (m@(pr,_,_):ms) =
      if matchValid (matched pr) removed
      then m : takeValid (consumed pr ++ removed) ms
      else takeValid removed ms

    -- checks to see if the tuples a match depends on have been consumed by an earlier match
    matchValid :: Dependency -> Consumed -> Bool
    matchValid deps ts = not $ any (`elem` ts) deps

    -- Disabling this for now: I don't like how its behavior is so closely tied
    -- to the order that step3 processes events. It would be more reasonable at
    -- the level of step4.
    --
    -- Instead, the "first" match will always win out, consuming the tuple.
    -- Order of matches within a call to `solve` still generally unspecified.
    --
    -- Prevents any members of a match group (a set of matches from a
    -- particular Trigger) that consume the same tuple from firing.
    -- TODO: should this cause a runtime error?
    --removeConflicts :: [Match] -> [Match]
    --removeConflicts matches = filter matchOK matches
    --  where
    --    removed = concatMap (consumed . fst) matches
    --    doubles = findDoubles removed
    --    matchOK = not . any (`elem` doubles) . consumed . fst
    --    findDoubles = map head . filter ((> 1) . length) . group . sort

applyLookups c es = second reverse <$> foldM step (c, []) es
  where
    step (c0, acc) expr = do
      (val, c1) <- applyLookup (reduce c0 expr) c0
      return (c1, val:acc)

-- TODO uncertain of negative semantics
applyMatch :: Match -> M2 ([Msg], Context)
applyMatch (prov, ctxt, forced) =
    case forced of
      [] -> do
        (ms1, c1) <- foldM applyStep ([], ctxt) implication
        return (reverse ms1 ++ removed, c1)
      -- If a match contains "proposed" negative tuples, it hasn't succeeded,
      -- but the negative tuples might enable a match.
      -- We create and emit them alone.
      _ -> do
        (ms2, c2) <- foldM force ([], ctxt) forced
        return (reverse ms2, c2)
  where
    removed = map (MT Negative) $ consumed prov

    implication = rhs $ ranked_rule $ rule_src prov

    applyStep :: ([Msg], Context) -> Assert -> M2 ([Msg], Context)
    applyStep (ms, c0) (Assert label exprs) = do
      (c1, nodes) <- applyLookups c0 exprs
      t <- packTuple (label, nodes) prov
      return (MT Positive t : ms, c1)

    validForced = const True

    -- TODO could generate fresh ids here if we didn't force it to be fully bound at the query stage
    force (ms, c) (fact, val) = do
      t <- packTuple fact prov
      -- TODO dumb negation (-) trick: interacts with use of Set.toAscList later.
      -- We want to process forced negatives before other tuples; not sure if strictly necessary.
      return (MT Positive t { tid = - (tid t), tval = val } : ms, c)
