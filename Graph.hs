module Graph where

import Control.Monad
import Data.List
import Data.Maybe
import Data.String

import Types
import Expr
import Parser2
import Parse (runParser)

-- interpret binary op
op2fn QEq = (==)
op2fn QDisEq = (/=)
op2fn QLess = (<)
op2fn QMore = (>)

constrainRelation l = filter ((== l) . label)

ref = NTRef

matchLookup :: NodeVar -> Context -> Maybe Node
matchLookup (NVal v) _ = Just v
matchLookup (NVar n) c = lookup n c
matchLookup NHole    c = Nothing

-- looks up value in context; generates fresh node if unbound
applyLookup :: Count -> NodeVar -> Context -> (Node, Count, Context)
applyLookup cnt v c | Just v <- matchLookup v c = (v, cnt, c)
applyLookup cnt (NVar n) c = (ref cnt, cnt+1, (n,ref cnt) : c)
applyLookup cnt NHole    c = (ref cnt, cnt+1,               c)

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
solveStep g (c, bound) (Query dot (EP linear e vs)) =
  -- 3 steps:
  --   constrainRelation picks out the relation tuples by name
  --   not . (`elem` bound) filters out tuples that have been bound "linearly" so far by this pattern
  --   edgeMatch unifies the variables of the Query against the nodes in a given tuple
  let pairs = mapMaybe (edgeMatch c vs)
         . filter (not . (`elem` bound))
         . constrainRelation e $ g
  in do
    (e, newC) <- pairs
    let newBound = if linear == Linear then e : bound else bound
    return (newC, newBound)

solveStep g (c, bound) (QBinOp op v1 v2) =
  case (matchLookup v1 c, matchLookup v2 c) of
    (Just v1', Just v2') -> if (op2fn op v1' v2') then [(c, bound)] else []
    _ -> error "(in)equality constraints must refer to bound values"

solveSteps :: Graph -> Bindings -> [Query] -> [Bindings]
solveSteps g c es = foldM (solveStep g) c es

solve :: Graph -> [Query] -> [Bindings]
solve g es = solveSteps g ([], []) es

applyStep :: (DBUpdate, Context, Int) -> Assert -> (DBUpdate, Context, Int)
applyStep (d@(DBU {new_tuples = es, new_id_counter = count0, new_tuple_counter = t_count}), c0, t)
          (Assert label exprs) =
  (DBU { new_tuples = new : es
       , new_id_counter = new_id_count
       , new_removed = new_removed d
       , new_tuple_counter = t_count + 1}
  , c1
  , t+1)
  where
    step (c, count, acc) expr =
      let val = reduce c expr
          (val', count', c') = applyLookup count val c
      in (c', count', val':acc)

    (c1, new_id_count, exprs') = foldl step (c0, count0, []) exprs
    new = T { nodes = reverse exprs', label = label, ts = Time [t], tid = t_count }

-- returns db containing (ONLY new edges, new object counter)
applyMatch :: DBUpdate -> Match -> (Context, DBUpdate)
applyMatch dbu ((ctxt, bound), rhs) =
  let (dbu', ctxt', _) = foldl' applyStep (dbu, ctxt, 0) rhs
  in (ctxt', dbu' { new_removed = bound ++ new_removed dbu', new_tuples = reverse $ new_tuples dbu' })

-- `rhs` is a description of new edges to add, given a context
-- add edges for each match simultaneously (according to timestamp)
-- updates are sequenced so that new nodes get unique ids
-- all updates get same top-level time (see fixt)
applyAll :: DB -> [Match] -> DBUpdate
applyAll (DB {tuple_counter = t_counter, id_counter = counter}) ms = dbu
  where
    init = DBU {new_tuples = [], new_removed = [], new_id_counter = counter, new_tuple_counter = t_counter}
    dbu = foldl' (\a b -> snd $ applyMatch a b) init ms

rn :: String -> Node
rn x =
  case reads x of
    [(i, "")] -> NTInt i
    _ -> NTNamed x

rq :: String -> Maybe Assert
rq s =
  case words s of
    "#" : _ -> Nothing
    _ -> case runParser rquery_ s of
           Left e -> error $ "error parsing graph file: " ++ show e
           Right (a, "") -> Just a
           _ -> error "error parsing graph file."

readG file = do
  f <- readFile file
  return $ mapMaybe rq . filter notComment . lines $ f

ppC :: Context -> String
ppC = wrap . intercalate "," . map t . reverse
  where
    t (a, v) = a++":"++show v
    wrap s = "[" ++ s ++ "]"
ppR = wrap . intercalate "," . map ppC
  where
    wrap s = "{" ++ s ++ "}"

ppDB (DB {tuples = g}) = mapM_ print g
