module Reflection where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

import Types
import Expr
import Graph
import Rules
import Parser2
import Parse
import Increment

-- TODO last.unfold is dumb
insertTuple :: Index -> (Label, [Node]) -> DB -> DB
insertTuple index edge db = scheduleDB . last $ unfold (stepS index) schedule
  where
    (db', tuple) = makeTuple db edge
    schedule = (0, [tuple], db')


uiRels = ["box", "child", "color", "clear", "hide"]

pullRells :: [Label] -> DB -> [Tuple]
pullRells ls db = filter ok (tuples db)
  where
    ok t = label t `elem` ls

actions :: DB -> Index -> Label -> [RawTuple]
actions db ind label = concatMap step triggers
  where
    triggers = indLookup label ind
    step  (_, _, _, (EP _ _ rel vs), pattern) = actions
      where
        bindings = solveSteps (tuples db) emptyMatchBindings (S.toList pattern)
        -- TODO a rule with a free variable in a user-action should cause an error
        -- it will fail at this fromJust
        bind (ctxt, _, _) = (rel, map (fromJust . flip matchLookup ctxt) vs)
        actions = map bind bindings

--flattenRawTuple :: [RawTuple] -> [RawTuple]
--flattenRawTuple = snd . foldl' step 0
--  where
--    step (count, ts) (l, ns) = (count+1, header : (nodes ++ ts))
