{-# LANGUAGE TupleSections #-}
module Increment where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.Char

import Types
import Graph
import Expr
import Parser2
import Parse
import qualified Queue as Q

import Debug.Trace

emptyIndex = M.empty

dotClauses :: LHS -> LHS
dotClauses = mapMaybe isdot
  where
    isdot (DotQuery e) = Just (Query e)
    isdot _ = Nothing

fixClauses = map fix
  where
    fix (DotQuery e) = Query e
    fix t = t

insertRule :: Rule -> Index -> Index
insertRule rule@(Rule lhs rhs) ind =
  case dotClauses lhs of
    [] -> foldr step ind lhs'
    cs -> foldr step ind cs
  where
    lhs' = fixClauses lhs
    pattern = S.fromList lhs'
    step q@(Query ep@(EP l _)) ind = 
      M.insertWith (++) l [(rule, ep, S.delete q pattern)] ind
    step q@(HashQuery ep@(EP l _)) ind =
      M.insertWith (++) l [(rule, ep, S.delete q pattern)] ind
    step _ ind = ind

indLookup label ind | Just v <- M.lookup label ind = v
indLookup _ _ = []

-- looks up rel in index, then completes the Pattern into Match
increment :: Tuple -> Index -> Graph -> [Match]
increment tuple ind g = concatMap step ts
  where
    ts = indLookup (label tuple) ind
    step (Rule _ rhs, p@(EP _ _), pattern) = 
      let --initialContext = bindEdge v1 v2 [] tuple
          --initialContext = solveStep [
          cs = do
            c <- solveStep [tuple] [] (Query p)
            foldM (solveStep g) c (S.toList pattern)
      in zip cs (repeat rhs)

--queueEdge :: (Label, [Node]) -> Schedule -> Schedule
--queueEdge (l, ns) (c, q, db) = (c, Q.snoc t q, db')
--  where
--    t = T {nodes = ns, label = l, ts = Time [time_counter db]}
--    db' = db { time_counter = time_counter db + 1 }

type Schedule = (Int, [Tuple], DB)
stepLimit = 100
emptySchedule = (0, [], emptyDB)
cons = (:)

stepS :: Index -> Schedule -> Maybe Schedule
stepS _ p@(c, _, _) | c > stepLimit = Nothing
stepS _ p@(_, q, _) | null q = Nothing
stepS index (c, q, db) = Just (c+1, q', db')
  where
    next = head q
    fixt t = t { ts = ts next `appT` ts t }
    dbu = applyAll db (increment next index (tuples db))
    q' = foldr (cons) (tail q) (map fixt $ new_tuples dbu)
    db' = DB { tuples = next : tuples db
             , time_counter = time_counter db + 1
             , id_counter = new_id_counter dbu }

-- step1 :: Index -> DB -> Match -> DB
-- step1 ind db (c, rhs) =
--   let schedule = (0

trans1 = do
    edges <- readG edgeFile
    let (ctxt, dbu) = applyMatch initdbu ([], edges)
        --queue = foldl (flip Q.snoc) Q.empty (new_tuples dbu)
        stack = new_tuples dbu
        schedule = (0, stack, emptyDB { id_counter = new_id_counter dbu })

    rules <- readRules ruleFile

    mapM_ print $ rules

    -- TODO!: proper ordering
    let index = foldr (insertRule) emptyIndex $ rules
        steps = map justDB $ unfold (stepS index) schedule
        result = last steps

    (mapM_ print . tuples) result
    print $ length (tuples result)
    return result
  where
    initdbu = DBU {new_tuples = [], new_id_counter = 0}
    edgeFile = "graph.txt"
    ruleFile = "rules.rules"
    justDB (_,_,db) = db

main = trans1 >> return ()
