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

actions :: DB -> Index -> Label -> [RawTuple]
actions db ind label = concatMap step triggers
  where
    triggers = indLookup (label, Positive) ind
    step  (_, _, (EP _ _ rel vs), pattern) = actions
      where
        bindings = solveSteps (tuples db) (facts db) emptyMatchBindings (S.toList pattern)
        -- TODO a rule with a free variable in a user-action should cause an error
        -- it will fail at this fromJust
        bind (ctxt, _, _) = (rel, map (fromJust . flip matchLookup ctxt) vs)
        actions = map bind bindings

attributes :: Node -> DB -> [Tuple]
attributes n db = filter ok . fromGraph $ tuples db
  where
    ok t = n `elem` nodes t
