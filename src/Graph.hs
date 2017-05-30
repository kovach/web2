{-# LANGUAGE RecordWildCards #-}
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

-- interpret binary op
op2fn QEq = (==)
op2fn QDisEq = (/=)
op2fn QLess = (<)
op2fn QMore = (>)

constrainRelation l g =
  case M.lookup l g of
    Nothing -> []
    Just x -> x

ref = NTRef

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

-- process one variable unification instance on the LHS
matchStep :: Context -> (Node, NodeVar) -> Maybe Context
matchStep c (node, (NVar n)) =
  case lookup n c of
    Nothing -> Just $ (n, node) : c
    Just v -> if v == node then Just c else Nothing
matchStep c (node, (NVal v)) = if v == node then Just c else Nothing
matchStep c (node, NHole) = Just c

-- process all unification instances for a given tuple
edgeMatch :: Context -> [NodeVar] -> Tuple -> Maybe (Tuple, Context)
edgeMatch c vs t | length (nodes t) /= length vs = error $ "relation/pattern arity mismatch! tuple involved:" ++ show t
edgeMatch c vs t =
  let ps = zip (nodes t) vs
  in do
    c' <- foldM matchStep c ps
    return (t, c')

solveStep :: Graph -> Bindings -> Query -> [Bindings]
solveStep g (c, bound, deps) (Query dot (EP linear unique e vs)) =
    -- 4 steps:
    --   constrainRelation picks out the relation tuples by name
    --   not . (`elem` bound) filters out tuples that have been bound "linearly" so far by this pattern
    --   edgeMatch unifies the variables of the Query against the nodes in a given tuple
    --   handleUnique returns a single tuple if the unique flag is set
    let pairs =
          handleUnique
          . mapMaybe (edgeMatch c vs)
          . filter (not . (`elem` bound))
          . constrainRelation e $ g
    in do
      (e, newC) <- pairs
      let newBound = if linear == Linear then e : bound else bound
      return (newC, newBound, e : deps)
  where
    handleUnique = if unique == Unique then safeInit else id
    safeInit [] = []
    safeInit (x:_) = [x]

solveStep g b@(c, bound, _) (QBinOp op v1 v2) =
  case (matchLookup (reduce c v1) c, matchLookup (reduce c v2) c) of
    (Just v1', Just v2') -> if (op2fn op v1' v2') then [b] else []
    _ -> error "(in)equality constraints must refer to bound values"

solveSteps :: Graph -> Bindings -> [Query] -> [Bindings]
solveSteps g c es = foldM (solveStep g) c es

-- Main Matching Function --
-- looks up rel in index, then completes the Pattern into Match
getMatches :: Tuple -> Index -> Graph -> [Match]
getMatches tuple ind g = takeValid [] $ concat result
  where
    ts = indLookup (label tuple) ind
    result = map step ts
    step :: Trigger -> [Match]
    step (ruleid, linear, Rule _ rhs, p@(EP _ _ _ _), pattern) =
      let cs = do
            let b0 = emptyMatchBindings
            -- bind new tuple to identified clause
            c <- solveStep (toGraph [tuple]) b0 (Query Low p)
            -- match remaining clauses
            solveSteps g c (S.toList pattern)
          fixMatch binding = (ruleid, binding, rhs)
          matches = map fixMatch cs
      in case linear of
           Linear -> removeConflicts matches
           NonLinear -> matches

applyStep :: Provenance -> Context -> Assert -> M2 Context
applyStep prov c0 (Assert label exprs) = do
    (c1, nodes) <- foldM step (c0, []) exprs
    scheduleAdd (label, reverse nodes) (Just prov)
    return c1
  where
    step (c0, acc) expr = do
      (val, c1) <- applyLookup (reduce c0 expr) c0
      return (c1, val:acc)

applyMatch :: Match -> M2 Context
applyMatch (ruleid, (ctxt, bound, deps), rhs) = do
  c <- foldM (applyStep (ruleid, deps)) ctxt $ reverse rhs
  mapM scheduleDel bound
  return c

-- Main Update Function --
stepDB :: M2 Bool
stepDB = do
  modify $ \s -> s { gas = gas s - 1 }
  -- a_unprocessed, index, db
  IS{..} <- get
  case a_unprocessed of
    [] -> return True
    (r,p):xs -> do
      t <- makeTuple r p
      let ms = (getMatches t index (tuples db))
      modify $ \s -> s { a_unprocessed = xs }
      mapM_ applyMatch $ reverse ms
      storeTuple t
      processDels
      return False

fixDB :: M2 ()
fixDB = do
  g <- gets gas
  if g < 1 then return ()
  else do
    guard <- stepDB
    if guard then return () else fixDB

-- checks to see if the tuples a match depends on have been consumed by an earlier match
matchValid :: Bindings -> Consumed -> Bool
matchValid (_,_,deps) ts = not $ any (`elem` ts) deps

takeValid _ [] = []
takeValid removed (m@(_, bs@(_, consumed, deps),_):ms) =
  if matchValid bs removed then m : takeValid (consumed++removed) ms else takeValid removed ms

rn :: String -> Node
rn x =
  case reads x of
    [(i, "")] -> NTInt i
    _ -> NTNamed x

rq :: String -> Maybe Assert
rq s =
  case words s of
    -- TODO not needed?
    "#" : _ -> Nothing
    _ -> case runParser rquery_ s of
           Left e -> error $ "error parsing graph file: " ++ show e
           Right (a, "") -> Just a
           _ -> error "error parsing graph file."

readDBFile file = do
  f <- readFile file
  return $ mapMaybe rq . filterComment $ f
