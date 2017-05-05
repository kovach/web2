{-# LANGUAGE TupleSections #-}
module Increment where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import Data.List (sort, sortOn)
import qualified Data.Map as M

import System.Console.ANSI

import Data.Char

import Types
import Expr
import Graph
import Rules
import Parser2
import Parse

import Debug.Trace

emptyIndex = M.empty

dotClauses :: LHS -> LHS
dotClauses = mapMaybe isdot
  where
    isdot q@(Query High _) = Just q
    isdot _ = Nothing

linearClauses :: LHS -> LHS
linearClauses = mapMaybe islinear
  where
    islinear q@(Query _ (EP Linear _ _ _)) = Just q
    islinear _ = Nothing

insertRule :: (Int, Rule) -> Index -> Index
insertRule (id, rule@(Rule lhs rhs)) ind =
  case dotClauses lhs of
    [] -> foldr step ind lhs
    cs -> foldr step ind cs
  where
    pattern = S.fromList lhs
    -- For now, this makes rules trigger in order of appearance in file
    step q@(Query _ ep@(EP _ _ rel _)) ind =
      M.insertWith (++) rel [(id, linear, rule, ep, S.delete q pattern)] ind
    step _ ind = ind
    -- TODO remove
    linear = if not . null . linearClauses $ lhs then Linear else NonLinear

indLookup label ind | Just v <- M.lookup label ind = v
indLookup _ _ = []

findDoubles xs = step sorted
  where
    sorted = sort xs
    step [] = []
    step [x] = []
    step (x:y:ys) | x == y = x : step (dropWhile (== x) ys)
    step (_:ys) = step ys

-- prevents any members of a match group (a set of matches from a particular
-- Trigger) that consume the same tuple from firing
-- TODO: should this cause a runtime error?
removeConflicts :: [Match] -> [Match]
removeConflicts matches = filter matchOK matches
  where
    consumed = concatMap takeConsumed matches
    doubles = findDoubles consumed
    matchOK = not . any (`elem` doubles) . takeConsumed

-- looks up rel in index, then completes the Pattern into Match
increment :: Tuple -> Index -> [Tuple] -> [Match]
increment tuple ind g = concat result
  where
    ts = indLookup (label tuple) ind
    result = map step ts
    step :: Trigger -> [Match]
    step (ruleid, linear, Rule _ rhs, p@(EP _ _ _ _), pattern) =
      let cs = do
            -- bind new tuple to identified clause
            let b0 = emptyMatchBindings
            c <- solveStep [tuple] b0 (Query Low p)
            -- match remaining clauses
            foldM (solveStep g) c (S.toList pattern)
          fixMatch binding = (ruleid, binding, rhs)
          matches = map fixMatch cs
      -- TODO doesn't handle multiple matches of linear tuple correctly
      in case linear of
           Linear -> case matches of
             _ -> removeConflicts matches
           NonLinear -> matches

type Schedule = (Int, [Tuple], DB)
stepLimit = 100
emptySchedule = (0, [], emptyDB)
cons = (:)

-- Main function responsible for updating program state
--   - pops edge from stack
--   - selects rules it may trigger
--   - computes results of rules
--   - mutates db
stepS :: Index -> Schedule -> Maybe Schedule
stepS _ p@(c, _, _) | c > stepLimit = Nothing
stepS _ p@(_, q, _) | null q = Nothing
stepS index (c, q, db) = Just (c+1, q', db')
  where
    wut ts = 97 `elem` (map tid ts)
    next = head q
    fixt t = (if tid t == 97 then trace ("\n\nwut"++show t++"\n\n") else id) $ t { ts = ts next `appT` ts t }
    dbu = applyAll db (increment next index (tuples db))
    -- new tuples will be processed first
    q' = (map fixt $ new_tuples dbu) ++ (tail q)
    newly_removed = new_removed dbu
    db' = DB { tuples = filter (not . (`elem` newly_removed)) $ next : tuples db
             , removed_tuples = newly_removed ++ removed_tuples db
             , time_counter = time_counter db + 1
             , id_counter = new_id_counter dbu
             , tuple_counter = new_tuple_counter dbu
             }

-- Main function
trans1 edgeFile ruleFile = do
    edges <- readDBFile edgeFile
    let (ctxt, dbu) = applyMatch initdbu (0, emptyMatchBindings, edges)
        --queue = foldl (flip Q.snoc) Q.empty (new_tuples dbu)
        stack = new_tuples dbu
        schedule = (0, stack, emptyDB { tuple_counter = new_tuple_counter dbu, id_counter = new_id_counter dbu })

    rules <- readRules ruleFile

    mapM_ print $ rules
    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules

    -- TODO!: robust ordering
    let index = foldr (insertRule) emptyIndex $ zip [1..] rules
        steps = map justDB $ unfold (stepS index) schedule
        result = last steps
        log = sortOn (revT . ts . snd) $ (map (True,) $ tuples result) ++ (map (False,) $ removed_tuples result)

    -- setSGR [SetColor Foreground Vivid Blue]
    -- putStrLn "added:"
    -- (mapM_ print . tuples) result
    -- setSGR [SetColor Foreground Vivid Red]
    -- putStrLn "removed:"
    -- (mapM_ print . removed_tuples) result
    -- setSGR [Reset]

    putStrLn "\ngame log:"
    mapM_ (colorTuple rules) $ log

    --putStrLn "partial game log?"
    --mapM_ (colorTuple [rules !! 0]) $ log

    putStrLn "final tuple count:"
    print $ length (tuples result)

    -- random unit test
    let alltids = sort $ map tid (tuples result ++ removed_tuples result)
    if ([0..length alltids - 1] /= alltids)
      then do
        putStrLn "tids not consistent!!"
        print alltids
        print $ zipWith (-) alltids (tail alltids)
        print $ map tid $ tuples result
        print $ map tid $ removed_tuples result
      else putStrLn "tids consistent!"

  where
    initdbu = DBU {new_tuples = [], new_removed = [], new_id_counter = 0, new_tuple_counter = 0}
    justDB (_,_,db) = db
    colorTuple rules (alive, t) = do
      let debug = True
      let ddebug = True
      let str = (if alive then "" else "    ") ++ ppTuple t
      case tupleIOType rules t of
        Input -> do
          setSGR [SetColor Foreground Vivid White]
          putStrLn str
        Output -> do
          setSGR [SetColor Foreground Dull Blue]
          putStrLn str
        Internal -> unless (not debug) $ do
          setSGR [SetColor Foreground Dull Green]
          putStrLn str
        Ignored -> unless (not ddebug) $ do
          setSGR [SetColor Foreground Dull Yellow]
          putStrLn str
      setSGR [Reset]

main = trans1 "graph.txt" "rules.arrow"
main2 = trans1 "graph2.txt" "parser.arrow"
main3 = trans1 "graph3.txt" "linearity.arrow"
