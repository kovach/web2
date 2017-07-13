-- TODO
--  ? watch lists
--  ! detect proof cycles; ensure the model is always minimal
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.List (partition, nub)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Data.Either (partitionEithers)
import Control.Monad
import Control.Monad.State

import Types
import FactIndex
import Rules
import Index (isLinear)
import Monad
import Graph (getMatches, applyMatch, applyLRHS)

import Debug.Trace

-- Types --

-- Each rule has a local view of
--  [0] the graph (event tuples)
--  [1] the proof state for its input relations
--  [2] the proof state for its output relations
-- When a ~> rule computes new matches, or invalidates old ones, the changes
-- are immediately applied to its output FS and added to the Msg output list.
--   In particular, if those relations are also in the input set, they are not added until
--   they are processed into Events at a later step
type DBL = (Graph, FactState, FactState)

type RankedRule = (Int, Rule)

-- Things we just want to compute once
data RuleFlags = RF
  { ruleLinear :: Bool
  , ruleHasPair :: Bool
  , prioritySig :: [Signature]
  }

-- We use `queues` as a priority queue.
-- Each rule is ranked by an Int, so `M.elemAt 0` returns the first non-empty
-- queue (see initS, step)
data S = S
  { dependencies :: Map Label [RankedRule]
  , queues :: Map RankedRule [Msg]
  , localDB :: Map Rule DBL
  , flags :: Map Rule RuleFlags
  }

-- inner state: (inputs, state, local table, outputs)
type IS = ([Event], DBL, [Msg])

-- Utilities --

pushMsg :: Msg -> S -> S
pushMsg msg s@S{..} = s { queues = queues' }
  where
    queues' = foldr (\r -> M.insertWith ((++)) r [msg])
                    queues (lookList (mlabel msg) dependencies)

pushMsgs :: [Msg] -> S -> S
pushMsgs ms s = foldr pushMsg s ms

-- used to update global DB
updateDB :: Msg -> DB -> DB
updateDB (MT Positive t) db = db { tuples = insertTuple t (tuples db) }
updateDB (MT Negative t) db = db { tuples = removeTuple t (tuples db)
                                 , removed_tuples = t : removed_tuples db }
updateDB f db = db { facts = updateFact f (facts db) }

commitMsgs :: [Msg] -> M2 ()
commitMsgs [] = return ()
commitMsgs msgs = do
  d <- gets db
  let db' = foldr updateDB d msgs
  moddb $ const db'
  mapM_ outputMsg msgs

-- ~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Here begins The Algorithm --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~ --
--   important algorithm properties:
--     - a new tuple is added to a given queue at most once (by the rule which generates it) (see step)
--     ! new msgs are added to the front of the "queue"
--       - this is important only for implementing => rule consumption priority
--         prevents a match on a tuple that has already been consumed
--       ! otherwise, ordering of events is supposed to be irrelevant
--     - step2 generates at most 1 event per fact


hasAntipodePair :: Eq a => [(a, Polarity)] -> Bool
hasAntipodePair [] = False
hasAntipodePair ((l,p):rest) | (l, neg p) `elem` rest = True
hasAntipodePair (_:rest) = hasAntipodePair rest

initS :: [Rule] -> [Msg] -> DB -> S
initS rs ms db = pushMsgs ms s0
  where
    rrs = zip [0..] rs
    s0 = S
      { dependencies = step1 rrs
      , queues = M.empty
      , localDB = M.fromList $ zip rs $ map fix rs
      , flags = M.fromList $ map ruleFlags rs }
    fs = facts db
    ts = tuples db
    fix r = (ts, fs, restrictFacts r fs)

    ruleFlags rule = (rule, rf)
      where
        opposites = nub $ mapMaybe op (lhs rule)
        rf = RF { ruleLinear = linear, ruleHasPair = dangerous, prioritySig = opposites }
        op (Query _ p) = Just (epLabel p, neg (epSign p))
        op _ = Nothing
        linear = isLinear rule
        -- such rules need an extra check in step3
        dangerous = hasAntipodePair opposites

    step0 :: RankedRule -> Map Label [RankedRule]
    step0 r@(_, rule) = M.fromList $ zip (lhsRels rule) (repeat [r])

    step1 :: [RankedRule] -> Map Label [RankedRule]
    step1 = foldr (M.unionWith (++)) M.empty . map step0

-- New tuples (that haven't been consumed) pass through as events
-- Proofs are combined into fs1; the diff wrt fs0 passes through as events
-- Any other proofs (those not contributing to visible events) are added immediately to fs0
step2 :: [Msg] -> FactState -> (FactState, [Event])
step2 msgs fs0 = (fs0', events)
  where
    split (MT p t) = Left (E p t)
    split f@(MF _ _ _) = Right f
    (tuples, proofs) = partitionEithers $ map split msgs

    removeConsumed (E Negative t) = filter (/= E Positive t)
    removeConsumed _ = id

    tuples' = foldr removeConsumed tuples tuples

    fs1 = foldr updateFact fs0 proofs
    fevents = fsDiff fs0 fs1
    -- primary output of this function
    events = fevents ++ tuples'

    -- These are proofs that need to be recorded but aren't part of any event;
    -- without this rule for fs0', they would be lost.
    msgs' = filter (not . (`elem` (map elabel fevents)) . mlabel) proofs
    fs0' = foldr updateFact fs0 msgs'

-- Returns true if pr assumes anything *counter to* e
dependent' :: Event -> Provenance -> Bool
dependent' e pr = any (check e) (matched pr)
  where
    check (EFalse f) (EFact f' _) | f == f' = True
    check (EFact f _) (EFalse f') | f == f' = True
    check (E p t) (E p' t') | t == t' && p == neg p' = True
    check _ _ = False

-- Main Evaluation step --

-- effects of an event:
--   "negative":
--      remove (positive) dependents from out
--      for LRule, remove dependents from me
--        triggers output
--      E: remove from graph
--   "positive":
--      get new matches
--        (for ~>) add to me, and to output
--        (for =>) add to output
--      E: add to graph
--      EFact: add proofs to fs
--      EFalse: remove all proofs from fs

step3 :: Bool -> Event -> Rule -> IS -> M2 IS
step3 marked ev rule (es, (g, fs, me), out) =
  case rule of
    Rule lhs rhs -> do
        mapM_ applyMatch matches2
        new <- flushEvents
        let g2 = foldr removeConsumed g1 new
        return (es', (g2, fs', me), new ++ out)
      where
        removeConsumed (MT Negative t) g = removeTuple t g
        removeConsumed _ g = g

    LRule lhs rhs -> do
        return (es', (g1, fs', me2), new ++ out)
      where
        added = concatMap applyLRHS matches2
        -- (proofs refuted by ev, proofs unaffected)
        (falsified, me1) = falsify ev me
        me2 = foldr updateFact me1 added
        new = falsified ++ added
  where
    matches1 = getMatches ev rule g fs
    matches2 =
      if marked
           -- if the match depends on both (p x) and (!p x), filter it out
      then filter consistent .
           -- if a rule is not marked, its input events have been ordered such
           -- that this filter is unecessary
           -- small optimization: only needs to be done on "high" prefix of input events
           --
           -- if a later event rejects the match, filter it out:
           filter (\m -> not $ any (rejects m) es') $ matches1
      else matches1

    g1 = case ev of
           E Positive t -> insertTuple t g
           E Negative t -> removeTuple t g
           _ -> g

    fs' = updateEv ev fs

    -- TODO remove this from fold state
    es' = es

    rejects (pr, _) ev = dependent' ev pr

    -- TODO this should be done in Graph
    consistent (pr, _) = not . hasAntipodePair . map fix . matched $ pr
      where
        fix e = (efact e, epolarity e)

-- call step3 on each event
-- accumulates local state changes and output msgs
step4 :: Bool -> [Event] -> Rule -> DBL -> M2 (DBL, [Msg])
step4 marked es r dbl = go (es, dbl, [])
  where
    go :: IS -> M2 (DBL, [Msg])
    go ([], dbl, out) = do
      return (dbl, out)
    -- step3 never adds to es
    go (e:es, dbl, ts) = step3 marked e r (es, dbl, ts) >>= go

step :: S -> M2 (Maybe S)
step (s@S{queues, localDB, flags}) =
    -- TODO upgrade to 0.5.10, use lookupMin?
    if M.size work == 0 then return Nothing else do
        -- eval
        (dbl', output) <- step4 dangerous events1 rule dbl2
        -- add to primary record, external output
        commitMsgs output
        unless (null output) $ do
          logMsg "---\ninputs:"
          mapM_ (logMsg . ("  "++) . ppMsg) msgs
          logMsg (show rule)
          mapM_ (logMsg . ("  "++) . ppMsg) output
          logMsg "---end---"
        let s1 = s
              -- empty the queue
              { queues = M.insert rr [] work
              -- update its local state
              , localDB = M.insert rule dbl' localDB }
        -- add to queues
        let s2 = pushMsgs output s1
        return $ Just s2
  where
    -- always ok to remove empty queues
    work = clean queues

    -- Prepare for step4
    (rr@(_,rule), msgs) = M.elemAt 0 work -- laziness
    (g, fs0, me) = look rule localDB
    (fs1, events0) = step2 msgs fs0
    dbl2 = (g, fs1, me)
    (dangerous, events1) = reorderEvents events0

    -- We reorder inputs, putting events that could cause
    -- matches to fail first. This prevents spurious matches.
    -- This is impossible when `ruleHasPair` is True. An extra check in step3
    -- handles it.
    reorderEvents es =
      case M.lookup rule flags of
        Just (RF {ruleLinear, ruleHasPair, prioritySig}) ->
          let (high, low) = partition ((`elem` prioritySig) . toSig) es
          in (ruleHasPair, high ++ low)
        _ -> error $ "missing RuleFlags map data for rule:\n  " ++ show rule
    toSig e = (elabel e, epolarity e)

solve :: [Rule] -> [Msg] -> M2 S
solve rs msgs = do
    db <- gets db
    logMsg "solve"
    mapM_ (logMsg . ppMsg) msgs
    commitMsgs msgs
    let state = initS rs msgs db
    unfoldM state (\s -> withGas (step s))
  where
    unfoldM :: Monad m => a -> (a -> m (Maybe a)) -> m a
    unfoldM s f = do
      m <- f s
      case m of
        Nothing -> return s
        Just s' -> unfoldM s' f
