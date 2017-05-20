{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Index where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort, sortOn)

import Data.Char

import Types
import Expr
import Rules
import Parse

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


makeIndex :: [Rule] -> Index
makeIndex = foldr (insertRule) emptyIndex . zip [1..]

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

attributes :: Node -> DB -> [Tuple]
attributes n db = filter ok (tuples db)
  where
    ok t = n `elem` nodes t


type Schedule = (Int, [Tuple], DB)
scheduleDB (_, _, db) = db
stepLimit = 150
emptySchedule = (0, [], emptyDB)
