-- TODO
--  ? watch lists
--  ! detect proof cycles; ensure the model is always minimal
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.List (nub, partition, intercalate)
import Data.Maybe
import Data.Either (partitionEithers)
import Control.Monad
import Control.Monad.State

import Types
import Rules
import Index
import Monad
import Graph

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

-- We use queues as a priority queue
-- each rule is ranked by an Int, [0..], so `M.elemAt 0` returns the first
-- non-empty queue (see initS, step)
data S = S
  { edges :: Map Label [RankedRule]
  , queues :: Map RankedRule [Msg]
  , localDB :: Map Rule DBL
  }

-- inner state: (inputs, state, local table, outputs)
type IS = ([Event], DBL, [Msg])

-- Utilities --

look k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> error $ "hey: " ++ show k

look' k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> []

fsMerge :: FactState -> FactState -> FactState
fsMerge = M.unionWith (++)

partitionMap :: Ord k => (a -> Bool) -> Map k [a] -> (Map k [a], Map k [a])
partitionMap f m = out
  where
    a = M.map (filter f) m
    b = M.map (filter (not . f)) m
    out = (clean a, clean b)

updateFact (MF Positive f pr) fs = M.insertWith (++) f [pr] fs
updateFact (MF Negative f pr) fs = M.adjust (filter (/= pr)) f fs
updateFact _ fs = fs

pushMsg :: Msg -> S -> S
pushMsg msg s@S{..} = s { queues = queues' }
  where
    queues' = foldr (\r -> M.insertWith ((++)) r [msg])
                    queues (look' (mlabel msg) edges)

pushMsgs :: [Msg] -> S -> S
pushMsgs ms s = foldr pushMsg s ms

step0 :: RankedRule -> Map Label [RankedRule]
step0 r@(_, rule) = M.fromList $ zip (lhsRels rule) (repeat [r])

step1 :: [RankedRule] -> Map Label [RankedRule]
step1 = foldr (M.unionWith (++)) M.empty . map step0

-- used to update global DB
updateDB :: Msg -> DB -> DB
updateDB (MT Positive t) db = db { tuples = insertTuple t (tuples db) }
updateDB (MT Negative t) db = db { tuples = removeTuple t (tuples db)
                                 , removed_tuples = t : removed_tuples db }
updateDB f db = db { facts = updateFact f (facts db) }

commitMsgs :: Maybe Rule -> [Msg] -> M2 ()
commitMsgs _ [] = return ()
commitMsgs mr msgs = do
  d <- gets db
  let db' = foldr updateDB d msgs
  moddb $ const db'
  logMsg (NULL mr)
  mapM_ logMsg msgs

-- ~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Here begins The Algorithm --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~ --
--   important algorithm properties:
--     - new tuple is added to a given queue at most once (by the rule which generates it) (see step)
--     ! new msgs are added to the front of the "queue"
--       - this is important only for implementing => rule consumption priority
--         prevents a match on a tuple that has already been consumed
--       ! otherwise, ordering of events is supposed to be irrelevant
--     - step2 generates at most 1 event per fact

initS :: [Rule] -> [Msg] -> DB -> S
initS rs ms db = pushMsgs ms s0
  where
    rrs = zip [0..] rs
    s0 = S
      { edges = step1 rrs
      , queues = M.empty
      , localDB = M.fromList $ zip rs $ map restrictFacts rs
      }
    fs = facts db
    ts = tuples db
    restrictFacts r@(LRule _ _) = (ts, fs, clean $ M.map (filter ((== r) . rule_src)) fs)
    restrictFacts r = (ts, fs, emptyFS)

fsDiff :: FactState -> FactState -> [Event]
fsDiff fs0 fs' = map (fix Negative) gone ++ map (fix Positive) new
  where
    fs = clean fs0
    initial  = M.keysSet fs
    -- true things that became false
    gone = S.toList $ M.keysSet (M.filter null fs') `S.intersection` initial
    -- things that became true
    new = S.toList $ M.keysSet (clean fs') \\ initial
    -- turn Fact into Event
    fix Positive key = EFact key (look key fs')
    fix Negative key = EFalse key

-- New Tuples (that haven't been consumed) pass through as events
-- Proofs are combined into fs1; the diff wrt fs0 passes through as events
-- Any other proofs (those not contributing to visible events) are added immediately to fs0
step2 :: [Msg] -> Rule -> DBL -> (DBL, [Event])
step2 msgs rule (g, fs0, me) = ((g, fs0', me), events)
  where
    split (MT p t) = Left (E p t)
    split f@(MF _ _ _) = Right f
    (tuples, proofs) = partitionEithers $ map split msgs

    removeConsumed (E Negative t) es = filter (/= E Positive t) es
    removeConsumed _ es = es

    tuples' = foldr removeConsumed tuples tuples

    fs1 = foldr updateFact fs0 proofs
    fevents = fsDiff fs0 fs1
    -- the primary output of this function
    events = fevents ++ tuples'

    -- these are proofs that need to be recorded but aren't part of any event;
    -- without this rule for fs0', they would be lost
    msgs' = filter (not . (`elem` (map elabel fevents)) . mlabel) proofs
    fs0' = foldr updateFact fs0 msgs'

dependent' :: Event -> Provenance -> Bool
dependent' e pr = any (check e) (matched pr)
  where
    check (EFalse f) (EFact f' _) | f == f' = True
    check (EFact f _) (EFalse f') | f == f' = True
    check (E p t) (E p' t') | t == t' && p == neg p' = True
    check _ _ = False

positiveDependent' :: Event -> Msg -> Bool
positiveDependent' _ (MT Negative _) = False
positiveDependent' _ (MF Negative _ _) = False
positiveDependent' ev (MT _ t) = dependent' ev (source t)
positiveDependent' ev (MF _ _ pr) = dependent' ev pr

-- nb: use below relies on calls to clean in partitionMap
splitDeps ev = partitionMap (dependent' ev)

-- Main Evaluation step --

-- effects of an event:
--   "negative":
--      for E Negative, remove E Positive from es
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

step3 :: Event -> Rule -> IS -> M2 IS
step3 ev rule (es, (g, fs, me), out) =
  case rule of
    Rule lhs rhs -> do
        mapM_ applyMatch $  matches
        new <- flushEvents
        let g2 = foldr removeConsumed g1 new
        return (es', (g2, fs', me), new ++ out1)
      where
        ind = makeIndex [rule]
        matches = getMatches ev ind g fs
        removeConsumed (MT Negative t) g = removeTuple t g
        removeConsumed _ g = g

    LRule lhs rhs -> do
        return (es', (g1, fs', me2), new ++ out1)
      where
        ind = indexLRule rule
        added = getLMatches ev ind g fs
        -- (proofs refuted by ev, proofs unaffected)
        (dying, me1) = splitDeps ev me
        me2 = foldr updateFact me1 added
        falsified = map (uncurry $ MF Negative) $ concatMap flatten $ M.toList dying
        new = falsified ++ added
  where
    g1 = case ev of
           E Positive t -> insertTuple t g
           E Negative t -> removeTuple t g
           _ -> g

    fs' = case ev of
            EFact f prs -> M.insertWith (++) f prs fs
            EFalse f -> M.insert f [] fs
            _ -> fs

    -- TODO remove this from fold state
    es' = es

    -- Removes Positive Msgs that depend on the opposite of the current event
    out1 = filter (not . positiveDependent' ev) out

    flatten (a, bs) = zip (repeat a) bs

-- call step3 on each event
-- accumulates local state changes and output msgs
step4 :: [Event] -> Rule -> DBL -> M2 (DBL, [Msg])
step4 es r dbl = go (es, dbl, [])
  where
    go :: IS -> M2 (DBL, [Msg])
    go ([], dbl, out) = do
      commitMsgs (Just r) out
      return (dbl, out)
    -- step3 never adds to es
    go (e:es, dbl, ts) = step3 e r (es, dbl, ts) >>= go

step :: S -> M2 (Maybe S)
step (s@S{queues, localDB}) =
    -- TODO upgrade to 0.5.10, use lookupMin?
    if M.size work == 0 then return Nothing else
    let (rr@(_,rule), msgs) = M.elemAt 0 work
        dbl1 = look rule localDB
        (dbl2, events) = step2 msgs rule dbl1
    in do
        mapM_ logMsg msgs
        -- eval
        (dbl', output) <- step4 events rule dbl2
        let s1 = s
              -- empty rule's queue
              { queues = M.insert rr [] work
              -- update its local state
              , localDB = M.insert rule dbl' localDB }
        -- apply?
        let s2 = pushMsgs output s1
        return $ Just s2
  where
    -- always ok to remove empty queues
    work = clean queues


solve :: [Rule] -> [Msg] -> M2 S
solve rs msgs = do
    db <- gets db
    commitMsgs Nothing msgs
    let state = initS rs msgs db
    unfoldM state (\s -> withGas (step s))
  where
    unfoldM :: Monad m => a -> (a -> m (Maybe a)) -> m a
    unfoldM s f = do
      m <- f s
      case m of
        Nothing -> return s
        Just s' -> unfoldM s' f

