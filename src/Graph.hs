-- TODO
--  issue: new tuple cannot match multiples slots of a pattern
--    (but old tuples can)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Graph where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.String

import Types
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

-- process all unification instances for a given tuple
edgeMatch :: Context -> [NodeVar] -> [Node] -> Maybe Context
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

solvePattern :: [Event2] -> Bindings -> Linear -> [NodeVar] -> [Bindings]
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
    handleUnique $ solvePattern (map toEvent $ constrainRelation e g) b linear vs
  where
    --handleUnique = if unique == Unique then safeInit else id
    handleUnique = id
    safeInit [] = []
    safeInit (x:_) = [x]

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
    es = filter ((== e) . elabel) $ toEvents fs
    raw (EFact f _) = f

solveStep _ _ b@(c, _, _) (QBinOp op v1 v2) =
    case (matchLookup (reduce c v1) c, matchLookup (reduce c v2) c) of
      (Just v1', Just v2') -> assert (op2fn op v1' v2') b
      _ -> error "(in)equality constraints must refer to bound values"
  where
    -- interpret binary op
    op2fn QEq = (==)
    op2fn QDisEq = (/=)
    op2fn QLess = (<)
    op2fn QMore = (>)

solveSteps :: Graph -> FactState -> Bindings -> [Query] -> [Bindings]
solveSteps g fs c es = foldM (solveStep g fs) c es

-- Main matching function --
getMatches :: Event2 -> Index -> Graph -> FactState -> [Match]
getMatches ev ind g fs = takeValid [] $ concatMap step triggers
  where
    pol = epolarity ev
    triggers = indLookup (elabel ev, pol) ind
    step :: Trigger -> [Match]
    step (linear, rule@(Rule _ rhs), p, pattern) = result
      where
        cs = do
          let b0 = emptyMatchBindings
          -- bind new tuple to identified clause
          b <- solvePattern [ev] b0 (epLinear p) (epNodes p)
          -- match remaining clauses
          solveSteps g fs b (S.toList pattern)
        toMatch (ctxt, consumed, matched) = (Provenance rule (Just ev) matched consumed, ctxt)
        matches = map toMatch cs
        result = case linear of
                    Linear -> removeConflicts matches
                    NonLinear -> matches

    takeValid :: Consumed -> [Match] -> [Match]
    takeValid _ [] = []
    takeValid removed (m@(pr,_):ms) =
      if matchValid (matched pr) removed
      then m : takeValid (consumed pr ++ removed) ms
      else takeValid removed ms


applyMatch :: Match -> M2 Context
applyMatch (prov, ctxt) = do
    let bound = consumed prov
        rhs = rhsRule $ rule_src prov
    c <- foldM (applyStep prov) ctxt $ reverse rhs
    mapM_ scheduleDel bound
    return c
  where
    applyStep :: Provenance -> Context -> Assert -> M2 Context
    applyStep prov c0 (Assert label exprs) = do
        (c1, nodes) <- foldM step (c0, []) exprs
        t <- makeTuple (label, reverse nodes) prov
        scheduleAdd t
        return c1
      where
        step (c0, acc) expr = do
          (val, c1) <- applyLookup (reduce c0 expr) c0
          return (c1, val:acc)

-- Takes positive Tuple or pos/neg LTuple; returns new proofs
getLMatches :: Event2 -> Index -> Graph -> FactState -> [Msg]
getLMatches ev ind g fs = map toMsg . concatMap step $ triggers
  where
    toMsg (f, p) = MF Positive f p
    triggers = indLookup (elabel ev, epolarity ev) ind
    step :: Trigger -> [(Fact, Provenance)]
    step (linear, rule, lp, pattern) =
      case edgeMatch [] (epNodes lp) (enodes ev) of
        Just ctxt -> concat $ do
          (ctxt, [], matched) <- solveSteps g fs (ctxt, [], [ev]) (S.toList pattern)
          return $ applyLRHS (Provenance rule (Just ev) matched [], ctxt)
        Nothing -> error "impossible"
    applyLRHS :: Match -> [(Fact, Provenance)]
    applyLRHS (prov, ctxt) = map step rhs
      where
        rule@(LRule _ rhs) = rule_src prov
        step (Assert l es) = ((l, map (simpleLookup rule ctxt) es), prov)

        simpleLookup :: Rule -> Context -> E -> Node
        simpleLookup rule c e =
          case matchLookup (reduce c e) c of
            Nothing -> error $ "Right-hand side of functional rule cannot contain holes or unbound variables. Offending expression: " ++ show e ++ "\n  in rule\n" ++ show rule
            Just n -> n

-- checks to see if the tuples a match depends on have been consumed by an earlier match
matchValid :: Dependency -> Consumed -> Bool
matchValid deps ts = not $ any (`elem` ts) (mapMaybe fromEvent deps)
  where
    fromEvent (E _ t) = Just t
    fromEvent _ = Nothing
