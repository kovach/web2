-- TODO
--  semantics issue: new tuple cannot match multiples slots of a pattern
--    (but old tuples can)
--
--  make order that tuples are added/processed from RHS more consistent
--    just need to move packTuple/scheduleAdd out of applyStep
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Graph where

import Control.Monad
import Control.Monad.State
import Data.List (group, sort)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.String

import Types
import FactIndex
import Expr
import Parser
import Parse (runParser)
import Monad
import Index

import Debug.Trace

assert :: Bool -> a -> [a]
assert True a = [a]
assert False _ = []

constrainRelation :: Label -> Graph -> [Tuple]
constrainRelation l g =
  case M.lookup l g of
    Nothing -> []
    Just x -> x

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
edgeMatch :: Context -> [NodeVar] -> [Node] -> Maybe Context
-- TODO check this statically
edgeMatch c vs nodes | length nodes /= length vs = error $ "relation/pattern arity mismatch! tuple involved:" ++ show nodes
edgeMatch c vs nodes =
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

solvePattern :: [Event] -> Bindings -> Linear -> [NodeVar] -> [Bindings]
solvePattern es (ctxt, bound, deps) linear nvs =
    let pairs =
          mapMaybe (\t -> fmap (t,) $ edgeMatch ctxt nvs (enodes t))
          . filter (not . (`elem` (map toEvent bound)))
          $ es
    in do
      (e, newC) <- pairs
      let newBound = if linear == Linear then etuple e : bound else bound
      return (newC, newBound, e : deps)

solveStep :: Graph -> FactState -> Bindings -> Query -> [Bindings]
solveStep g _ b@(c, bound, deps) (Query _ (EP linear unique e vs)) =
    solvePattern (map toEvent $ constrainRelation e g) b linear vs
  where
    --TODO remove
    --handleUnique = if unique == Unique then safeInit else id
    --safeInit [] = []
    --safeInit (x:_) = [x]

solveStep _ fs b@(c, bound, deps) q@(Query _ (LP polarity e ns)) =
  case polarity of
    Positive -> solvePattern es b NonLinear ns
    Negative ->
      let vs = mapM (\n -> matchLookup n c) ns in
      case vs of
        Nothing -> error $ "negation queries must refer to bound values:\n" ++ show q
        Just vs' -> assert (not $ (e, vs') `elem` (map raw es)) (c, bound, f : deps)
          where
            f = EFalse (e, vs')
  where
    es = toEvents e fs
    raw (EFact f _) = f
    raw _ = error "raw expects EFact"

solveStep _ _ b@(c, _, _) (QBinOp op v1 v2) =
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

solveSteps :: Graph -> FactState -> Bindings -> [Query] -> [Bindings]
solveSteps g fs c es = foldM (solveStep g fs) c es

-- Main matching function --
getMatches :: Event -> Rule -> Graph -> FactState -> [Match]
getMatches ev rule g fs = takeValid [] . map toMatch . go $ triggers
  where
    triggers = indLookup (elabel ev, epolarity ev) (indexRule rule)

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
        -- 3. Unify p2 against g/fs
        ps = map pat ts
        p1 = p : ps
        p2 = foldr S.delete pattern ps

        b0 = emptyMatchBindings
        bindings = do
          b1 <- foldM (\b (Query _ p) ->
                        solvePattern [ev] b (epLinear p) (epNodes p)) b0 p1
          solveSteps g fs b1 (S.toAscList p2)

    toMatch (ctxt, consumed, matched) =
      (Provenance rule (Just ev) matched consumed, ctxt)

    takeValid :: Consumed -> [Match] -> [Match]
    takeValid _ [] = []
    takeValid removed (m@(pr,_):ms) =
      if matchValid (matched pr) removed
      then m : takeValid (consumed pr ++ removed) ms
      else takeValid removed ms

    -- checks to see if the tuples a match depends on have been consumed by an earlier match
    matchValid :: Dependency -> Consumed -> Bool
    matchValid deps ts = not $ any (`elem` ts) (mapMaybe fromEvent deps)
      where
        fromEvent (E _ t) = Just t
        fromEvent _ = Nothing

    -- Disabling this for now: I don't like how its behavior is so closely tied
    -- to the order that step3 processes events. It would be more reasonable at
    -- the level of step4. How to implement that may be more clear once the
    -- `out1` problem is resolved.
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

applyMatch :: Match -> M2 Context
applyMatch (prov, ctxt) = do
    let bound = consumed prov
        rhs = rhsRule $ rule_src prov
    c <- foldM (applyStep prov) ctxt $ rhs
    --c <- foldM (applyStep prov) ctxt $ reverse rhs
    mapM_ scheduleDel bound
    return c
  where
    applyStep :: Provenance -> Context -> Assert -> M2 Context
    applyStep prov c0 (Assert label exprs) = do
        (c1, nodes) <- foldM step (c0, []) exprs
        t <- packTuple (label, reverse nodes) prov
        scheduleAdd t
        return c1
      where
        step (c0, acc) expr = do
          (val, c1) <- applyLookup (reduce c0 expr) c0
          return (c1, val:acc)

applyLRHS :: Match -> [Msg]
applyLRHS (prov, ctxt) = map (toMsg . step) rhs
  where
    toMsg (f, p) = MF Positive f p
    rule@(LRule _ rhs) = rule_src prov
    step (Assert l es) = ((l, map (simpleLookup rule ctxt) es), prov)

    simpleLookup :: Rule -> Context -> E -> Node
    simpleLookup rule c e =
      case matchLookup (reduce c e) c of
        Nothing -> error $ "Right-hand side of functional rule cannot contain holes or unbound variables. Offending expression: " ++ show e ++ "\n  in rule\n" ++ show rule
        Just n -> n
