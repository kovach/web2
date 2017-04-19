module Graph where

import Control.Monad
import Data.List
import Data.Maybe
import Data.String

import Types
import Expr

-- interpret binary op
op2fn QEq = (==)
op2fn QDisEq = (/=)
op2fn QLess = (<)
op2fn QMore = (>)

constrainRelation l = filter ((== l) . label)

ref = NTRef

matchLookup :: Q -> Context -> Maybe Node
matchLookup (QVal v) _ = Just v
matchLookup (QVar n) c = lookup n c

applyLookup :: Count -> Q -> Context -> (Node, Count)
applyLookup cnt v c | Just v <- matchLookup v c = (v, cnt)
applyLookup cnt (QVar n) _ = (ref cnt, cnt+1)

constrainNode c acc n = filter $
  case matchLookup n c of
    Just v -> (== v) . acc
    Nothing -> const True

bindNode p e c =
  case matchLookup p c of
    Nothing ->
      let QVar n = p in (n, e) : c
    Just _ -> c

bindEdge n1 n2 c tuple = bindNode n1 (src tuple) . bindNode n2 (tgt tuple) $ c

solveStep :: Graph -> Context -> Query -> [Context]
solveStep g c (Query (EP e v1 v2)) =
  let es = (constrainNode c src v1)
         . (constrainNode c tgt v2)
         . constrainRelation e $ g
  in
    map (bindEdge v1 v2 c) es

solveStep g c (QBinOp op v1 v2) =
  case (matchLookup v1 c, matchLookup v2 c) of
    (Just v1', Just v2') -> if (op2fn op v1' v2') then [c] else []
    _ -> error "(in)equality constraints must refer to bound values"

solve :: Graph -> [Query] -> [Context]
solve g es = foldM (solveStep g) [] es

applyStep :: Context -> (DBUpdate, Int) -> RQuery -> (DBUpdate, Int)
applyStep c (d@(DBU {new_tuples = es, new_id_counter = cnt0}), t) (Assert label e1 e2) =
  (DBU {new_tuples = new : es, new_id_counter = cnt2}, t+1)
  where
    v1 = reduce c e1
    v2 = reduce c e2
    (v1', cnt1) = applyLookup cnt0 v1 c
    (v2', cnt2) = applyLookup cnt1 v2 c
    new = T { src = v1', tgt = v2', label = label, ts = (Time [t]) }

-- returns db containing (ONLY new edges, new object counter)
apply :: DBUpdate -> Match -> DBUpdate
apply dbu (ctxt, rhs) = fst $ foldl' (applyStep ctxt) (dbu, 0) rhs

-- `rhs` is a description of new edges to add, given a context
-- add edges for each match simultaneously (according to timestamp)
-- updates are sequenced so that new nodes get unique ids
-- all updates get same top-level time (see fixt)
applyAll :: DB -> [Match] -> DBUpdate
applyAll (DB {tuples = graph, time_counter = t, id_counter = counter}) ms =
    let
      init = DBU {new_tuples = [], new_id_counter = counter}
      dbu@(DBU {new_tuples = new, new_id_counter = new_id}) = foldl' apply init ms
      --es = map fixt new
    in dbu -- DB {tuples = es ++ graph, time_counter = t+1, id_counter = new_id}
  --where
  --  fixt e@(T {ts = (Time ts)}) = e { ts = Time (t:ts) }

--applyRule :: DB -> Rule -> DB
--applyRule db (Rule lhs rhs) =
--  let cs = solve (tuples db) lhs
--  in applyAll db (zip cs (repeat rhs))

--p1 :: [Query]
--p1 = [("R1", ("x", "y")), ("R2", ("y", "z"))]
--p2 :: [Query]
--p2 = [("R1", ("a", "b")), ("R2", ("z", "b"))]
p5, p6 :: [Query]
p5 = concat $ [query "HP" "m" "hp", query "HP" "n" "hp", ineq QDisEq "m" "n"]
p6 = concat $ [query "HP" "m" "hp", query "HP" "n" "hp", ineq QEq "m" "n"]
p7 = concat $ [query "r" "a" "b", dquery "r" "b" "c"]
p8 = concat $ [query "R" "a" "b", query "R" "b" "c"]

--tor (Query (EP l n1 n2)) = Assert l n1 n2
--rhs = map tor

rule1  = Rule p7 (rquery "r" "a" "c")
rule1' = Rule p8 (rquery "R" "a" "c")
rule2 = Rule (dquery "r" "a" "b") (rquery "Q" "a" "b")
rule3 = Rule (query "Q" "a" "b") (rquery "R" "b" "a")
rule4 = Rule (query "+" "a" "b") (concat $ [rquery "S" "x" ("a" + "b")])

t1 = subQ [("m", QVar "MMM")] p5

type Def = ([Name], [Query])

bind :: Def -> [Q] -> [Query]
bind (ps, qs) args | length ps == length args = subQ (zip ps args) qs
pred :: Label -> Q -> [Query]
pred l v = [Query $ EP l (QVal (NTNamed "")) v]
query :: Label -> Q -> Q -> [Query]
query l v1 v2 = [Query $ EP l v1 v2]
dquery :: Label -> Q -> Q -> [Query]
dquery l v1 v2 = [DotQuery $ EP l v1 v2]
rquery :: Label -> E -> E -> [RQuery]
rquery l v1 v2 = [Assert l v1 v2]
ineq :: Op -> Q -> Q -> [Query]
ineq l v1 v2 = [QBinOp l v1 v2]

sym = QVal . NTNamed
leaf_ = (["t"], query "tag" "t" (sym "leaf"))
leaf1_ = (["x"], concat $ [query "left" "x" "a", query "right" "x" "b", bind leaf_ ["a"], bind leaf_ ["b"]])

subQ :: [(Name, Q)] -> [Query] -> [Query]
subQ bs query = map fix query
  where
    fix q = foldl (flip subst1) q bs 
    subst1 (n,v) (Query (EP rel l r)) = Query $ EP rel (sub n v l) (sub n v r)
    subst1 (n,v) (QBinOp op l r) = QBinOp op (sub n v l) (sub n v r)
    sub n v (QVar n') | n == n' = v
    sub _ _ e = e

ps :: [[Query]]
ps = [p7]

toTuple (l, (s,t)) = 
  T {src = s, tgt = t, label = l, ts = Time [0,0]}

--g1 :: Graph
--g1 = map toTuple [
--   ("R1", ("A", "B")) ,("R1", ("B", "C")) ,("R1", ("C", "A"))
--  ,("R2", ("B", "C")) ,("R2", ("C", "B")) ,("R2", ("A", "B"))
--  ,("HP", ("M", 2)) ]

rn :: String -> Node
rn x =
  case reads x of
    [(i, "")] -> NTInt i
    _ -> NTNamed x

rq :: String -> Maybe (Label, Node, Node)
rq s =
  case words s of
    [rel, a, b] -> Just $ (rel, rn a, rn b)
    [pred, a] -> Just $ (pred, rootNode, rn a)
    _ -> Nothing

readG file = do
  f <- readFile file
  return $ mapMaybe rq $ lines f

ppC :: Context -> String
ppC = wrap . intercalate "," . map t . reverse
  where
    t (a, v) = a++":"++show v
    wrap s = "[" ++ s ++ "]"
ppR = wrap . intercalate "," . map ppC
  where
    wrap s = "{" ++ s ++ "}"

initDB g = DB { tuples = g, time_counter = 0, id_counter = 0 }
emptyDB = initDB []

ppDB (DB {tuples = g}) = mapM_ print g

--main = do
--  g1 <- readG
--  let db0 = initDB g1
--  mapM_ (putStrLn . ppR . solve g1) $ ps
--  putStrLn "----------------"
--  ppDB db0
--  putStrLn "----------------"
--  let db1 = applyRule db0 rule1
--  ppDB $ db1
--  putStrLn "----------------"
--  let db2 = applyRule db1 rule1
--  ppDB $ db2
