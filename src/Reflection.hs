-- Internal syntax notes
-- 1. rule syntax:
--   rule r
--     imperative r
--     logical r
--   query q r
--     linear q
--     negated q
--   bin-op b r
--   assert q r
--
--   (query q _)
--     label q l
--     var v q n
--     literal l q n
--     hole q n
--
-- 2. tuple syntax:
--   label
--   node
--   tid t id
--   provenance t p
--     rule rule p
--     (maybe) cause p event
--     (m-m) matched event p
--     (m-1) consumed tuple p
--
--     ? context
--     !? TODO add unique id to Provenance, Event
--
--   event e
--     polarity e p
--     (maybe) e-tuple e tuple
--     (m-m) e-proof prov e
--       (mm, not m1, because ~> can have multiple clauses on rhs)
--
{-# LANGUAGE OverloadedStrings #-}
module Reflection where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

import Types
import Expr
import Monad
import Graph
import Index

-- TODO update
actions :: DB -> Index -> Label -> [RawTuple]
actions db ind label = concatMap step triggers
  where
    triggers = indLookup (label, Positive) ind
    step  (_, _, Query _ (EP _ _ rel vs), pattern) = actions
      where
        bindings = solveSteps (tuples db) (facts db) emptyMatchBindings (S.toList pattern)
        -- TODO a rule with a free variable in a user-action should cause an error
        -- it will fail at this fromJust
        bind (ctxt, _, _) = (rel, map (fromJust . flip matchLookup ctxt) vs)
        actions = map bind bindings
    step _ = error "impossible; no QBinOp in Index"

attributes :: Node -> DB -> [Tuple]
attributes n db = filter ok . fromGraph $ tuples db
  where
    ok t = n `elem` nodes t

-- ~~~~~~~~~~ --
-- Reflection --
-- ~~~~~~~~~~ --

makeT f p = do
  t <- packTuple (f, p) nullProv
  scheduleAdd t

flattenNV q c n (NVal node) = makeT "literal" [node, q, NInt n] >> return c
flattenNV q ctxt n (NVar name) =
  case lookup name ctxt of
    Just b -> do
      makeT "var" [b, q, NInt n]
      return ctxt
    Nothing -> do
      b <- freshNode
      makeT "var-name" [b, NSymbol name]
      makeT "var" [b, q, NInt n]
      return $ (name, b) : ctxt

flattenNV q c n (NHole) = makeT "hole" [q, NInt n] >> return c

flattenEP q c (EP lin _ l ns) = do
  let (L labelString) = l
  makeT "label" [q, NSymbol labelString]
  unless (lin == NonLinear) (makeT "linear" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenEP q c (LP pol l ns) = do
  let (L labelString) = l
  makeT "label" [q, NSymbol labelString]
  unless (pol == Positive) (makeT "negated" [q] >> return ())
  foldM (\a -> uncurry $ flattenNV q a) c (zip [1..] ns)

flattenQ r c (Query dot ep) = do
  q <- freshNode
  makeT "query" [q, r]
  flattenEP q c ep

-- TODO implement
flattenQ r c (QBinOp op e1 e2) = return c

-- TODO finish implementation
flattenE q c n (ELit i) = flattenNV q c n (NVal (NInt i))
flattenE q c n (ENamed str) = flattenNV q c n (NVal (NSymbol str))
flattenE q c n (EVar str) = flattenNV q c n (NVar str)
flattenE q c n (EHole) = flattenNV q c n NHole
flattenE q c n (EBinOp op e1 e2) = error "not implemented."
flattenE q c n (EString str) = error "not implemented."

flattenA r c (Assert l es) = do
  q <- freshNode
  makeT "assert" [q, r]
  let (L labelString) = l
  makeT "label" [q, NSymbol labelString]
  foldM (\c -> uncurry $ flattenE q c) c (zip [1..] es)

flattenRule :: Rule -> M2 ()
flattenRule rule = do
    r <- freshNode
    makeT "rule" [r]
    c <- foldM (flattenQ r) [] qs
    _ <- foldM (flattenA r) c as
    case rule of
      Rule _ _ -> makeT "imperative" [r]
      LRule _ _ -> makeT "logical" [r]
    return ()
  where
    qs = lhs rule
    as = rhs rule

-- TODO ! reflect tuples and provenance
