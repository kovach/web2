module Reflection where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

import Types
import Expr
import Graph
import Rules
import Parse
import Index

uiRels = ["box", "child", "color", "clear", "hide"]

--pullRells :: [Label] -> DB -> [Tuple]
--pullRells ls db = filter ok (tuples db)
--  where
--    ok t = label t `elem` ls

-- TODO generalize to sets of Label inputs?
-- For now, assume that actions are factored as single relations.
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
