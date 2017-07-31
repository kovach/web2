-- TODO
--  ? detect proof cycles; ensure the model is always minimal
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.List (partition, nub)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Either (partitionEithers)
import Control.Monad
import Control.Monad.State

import Types
--import FactIndex
import Rules
import Index (isLinear)
import Monad
import Graph (getMatches, applyMatch, applyLRHS)

import Debug.Trace
tr _ = id

-- Types --

-- TODO remove
-- Things we just want to compute once
--data RuleFlags = RF
--  { ruleLinear :: Bool
--  , ruleHasPair :: Bool
--  , prioritySig :: [Signature]
--  }
-- inner state: (inputs, state, local table, outputs)
--type IS = ([Event], DBL, [Msg])
--pushMsg :: Msg -> S -> S
--pushMsg msg s@S{..} = s { queues = queues' }
--  where
--    queues' = foldr (\r -> M.insertWith ((++)) r [msg])
--                    queues (lookList (mlabel msg) dependencies)

pushMsgs :: [Msg] -> M2 ()
pushMsgs = mapM_ pushMsgM

pushMsgM :: Msg -> M2 ()
pushMsgM m = modps $ pushMsg m

pushMsg :: Msg -> PS -> PS
pushMsg (MT p t) ps = ps { queues = queues' }
  where
    queues' = foldr (M.alter (step p)) (queues ps) actors
    actors = lookDefault (label t) (dependencies ps)
    -- insert
    step Positive Nothing = Just (MQ {m_pos = [t], m_neg = []})
    step Negative Nothing = Just (MQ {m_neg = [t], m_pos = []})
    -- enqueue
    step Positive (Just m@(MQ{m_pos})) = Just m { m_pos = t : m_pos }
    step a b = step2 a b
    step2 Negative (Just MQ {m_pos, m_neg}) =
      Just MQ { m_neg = t : m_neg, m_pos = filter (/= t) m_pos}

-- used to update global DB
updateDB :: Msg -> DB -> DB
updateDB (MT Positive t) db = db { tuples = insertTuple t (tuples db) }
updateDB (MT Negative t) db = db { tuples = removeTuple t (tuples db)
                                 , removed_tuples = t : removed_tuples db }
--updateDB f db = db { facts = updateFact f (facts db) }

commitMsgs :: [Msg] -> M2 ()
commitMsgs [] = return ()
commitMsgs msgs = do
  d <- gets db
  let msgs' = filter notRaw msgs
  let db' = foldr updateDB d msgs'
  moddb $ const db'
  mapM_ outputMsg msgs
  pushMsgs msgs

--TODO remove
--hasAntipodePair :: Eq a => [(a, Polarity)] -> Bool
--hasAntipodePair [] = False
--hasAntipodePair ((l,p):rest) | (l, neg p) `elem` rest = True
--hasAntipodePair (_:rest) = hasAntipodePair rest

-- TODO no DB parameter?
initPS :: [Rule] -> DB -> PS
initPS rs db = emptyProc
  { dependencies = M.unionWith (++) events rdeps
  , queues = M.empty
  , processors = M.fromList $ processors ++ reducers
  , relTypes = undefined
  }
  where
    rrs = zip [1..] rs
    graph = tuples db

    step0 :: RankedRule -> Map Label [Actor]
    step0 r@(i, rule) = M.fromList $ zip (lhsRels rule) (repeat [ARule i])

    step1 :: [RankedRule] -> Map Label [Actor]
    step1 = foldr (M.unionWith (++)) M.empty . map step0

    events = step1 rrs

    viewRels = logicalRelations rs

    rdep v = (toRaw v, [AReducer v])
    -- reducers bind "raw" tuples, output "reduced" tuples
    reducer v = (AReducer v, emptyReducer v)
    rdeps = M.fromList $ map rdep viewRels
    reducers = map reducer viewRels

    processor rr@(i, rule) =
      case rtype rule of
        Event -> (ARule i, ObsProc  rr graph)
        -- TODO need to populate watched map
        View  -> (ARule i, ViewProc rr graph M.empty)
    processors = map processor rrs

resetProcessor :: [Rule] -> M2 ()
resetProcessor rs = do
  d <- gets db
  modps (const (initPS rs d))


--TODO remove
--initS :: [Rule] -> [Msg] -> DB -> S
--initS rs ms db = pushMsgs ms s0
--  where
--    rrs = zip [1..] rs
--    s0 = S
--      { dependencies = step1 rrs
--      , queues = M.empty
--      , localDB = M.fromList $ zip rs $ map fix rs
--      , flags = M.fromList $ map ruleFlags rs }
--    fs = facts db
--    ts = tuples db
--    fix r = (ts, fs, restrictFacts r fs)
--
--    ruleFlags rule = (rule, rf)
--      where
--        opposites = nub $ mapMaybe op (lhs rule)
--        rf = RF { ruleLinear = linear, ruleHasPair = dangerous, prioritySig = opposites }
--        op (Query _ p) = Just (epLabel p, neg (epSign p))
--        op _ = Nothing
--        linear = isLinear rule
--        -- such rules need an extra check
--        dangerous = hasAntipodePair opposites
--
--    step0 :: RankedRule -> Map Label [RankedRule]
--    step0 r@(_, rule) = M.fromList $ zip (lhsRels rule) (repeat [r])
--
--    step1 :: [RankedRule] -> Map Label [RankedRule]
--    step1 = foldr (M.unionWith (++)) M.empty . map step0

{-
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

-- effects of an event:
--   "negative":
--      remove from graph
--      for View, remove dependents from me
--   "positive":
--      produce new matches
--      add to graph

step3 :: Bool -> Event -> RankedRule -> IS -> M2 IS
step3 marked ev rule (es, (g, fs, me), out) =
  case snd rule of
    -- TODO combine branches
    Rule lhs rhs -> do
        new <- concat . map fst <$> mapM applyMatch matches2
        let g2 = foldr removeConsumed g1 new
        return (es', (g2, fs', me), new ++ out)
      where
        removeConsumed (MT Negative t) g = removeTuple t g
        removeConsumed _ g = g

    LRule lhs rhs -> do
        added <- concat . map fst <$> mapM applyLRHS matches2
        let me2 = foldr updateFact me1 added
        let new = falsified ++ added
        return (es', (g1, fs', me2), new ++ out)
      where
        -- (proofs refuted by ev, proofs unaffected)
        (falsified, me1) = falsify ev me
  where
    matches1 = getMatches ev rule g fs
    matches2 =
      -- If a rule is not marked, the first filter is irrelevant, and its input
      -- events have been ordered such that the second filter is unecessary.
      if marked
           -- If the match depends on both (p x) and (!p x), filter it out:
      then filter consistent .
           -- TODO could also resolve this by decomposing `+p` into two messages:
           --   ! (!p): this falsifies !Fact patterns
           --   p: this produces new matches
           -- in rules, any instance of `p x, !p y` can be changed to `p' x, !p y`, with extra rule `p x ~> p' x`
           --
           -- small optimization: only needs to be done on "high" prefix of input events.
           -- If a later event rejects the match, filter it out:
           filter (\m -> not $ any (rejects m) es') $ matches1
      else matches1

    g1 = case ev of
           E Positive t -> insertTuple t g
           E Negative t -> removeTuple t g
           _ -> g

    fs' = updateEv ev fs

    -- TODO remove this from fold state
    es' = es
    -- TODO also remove out from fold state?

    rejects (pr, _) ev = dependent' ev pr

    -- TODO this should be done in Graph
    consistent (pr, _) = not . hasAntipodePair . map fix . matched $ pr
      where
        fix e = (efact e, epolarity e)

-- call step3 on each event
-- accumulates local state changes and output msgs
step4 :: Bool -> [Event] -> RankedRule -> DBL -> M2 (DBL, [Msg])
step4 marked es r dbl = go (es, dbl, [])
  where
    go :: IS -> M2 (DBL, [Msg])
    go ([], dbl, out) = do
      return (dbl, out)
    -- step3 never adds to es
    go (e:es, dbl, ts) = step3 marked e r (es, dbl, ts) >>= go

-}

takeQueue :: M2 (Maybe (Actor, MsgQueue))
takeQueue = do
  p <- gets processor
  let work = M.filter (not . isEmptyQueue) (queues p)
  if M.size work == 0 then return Nothing else do
    let (k, v) = M.elemAt 0 work
    modify $ \s -> s { processor = p { queues = M.insert k emptyQueue work } }
    return $ Just (k, v)

step :: M2 Bool
step = do
    p <- gets processor
    mwork <- takeQueue
    case mwork of
      Nothing -> return False
      Just (act, msgs) -> do
        let pr = look act (processors p)
        (output, pr') <- case pr of
          Reducer op l s vals -> stepReducer msgs l op s vals
          ObsProc r g -> stepRule msgs r g 
          ViewProc r g watched -> stepView msgs r g watched
        updateProc act pr'
        -- add to primary record, external output
        commitMsgs output
        return True

stepReducer mq l op state vals = tr (showIOM "reducer " ms outputs) $
  return (outputs, proc)
  where
    ms = toMsgs mq
    (state1, vals1, touched) = foldr step (state, fmap tval vals, S.empty) ms

    outputs = mapMaybe negVal changedf ++ newMsgs
    proc = Reducer op l state1 vals2

    look1 f vs =
      case M.lookup f vs of
        Nothing -> defaultVal op
        Just v -> tval v

    defaultVal Or = Truth False

    changedf = filter (\f -> look f vals1 /= (look1 f vals)) $ S.toList touched
    negVal f = case M.lookup f vals of
                 Just t -> Just $ MT Negative t
                 _ -> Nothing
    posVal f = (f, t)
      where
        p = Reduction op (look f state1)
        --t = (trueTuple (l, f) p)
        t = T f l p (look f vals1)
    newPairs = map posVal changedf
    newMsgs = map (MT Positive . snd) newPairs
    vals2 = foldr (uncurry M.insert) vals newPairs

    updateFact Positive t = M.insertWith (++) (nodes t) [t]
    updateFact Negative t = M.adjust (filter (/= t)) (nodes t)

    step (MT p t) (state, vals, touched) =
        (state1, M.insert f (update op) vals, S.insert f touched)
      where
        (_,f) = tfact t
        state1 = updateFact p t state
        update Or = case lookDefault f state1 of
                      [] -> Truth False
                      _ -> Truth True

updateProc a p = modps $ \ps -> ps { processors = M.insert a p (processors ps) }

-- TODO need to mark with value instead of tid
stepView :: MsgQueue -> RankedRule -> Graph -> WatchedSet -> M2 ([Msg], Processor)
stepView ms@(MQ {m_neg = neg}) rule g ws = do
    (new, ObsProc _ g1) <- stepRule ms rule g
    let new' = map rawFact new
    let (falsified, ws1) = falsify neg ws
        ws2 = foldr indexProof ws1 new'
    return (map (MT Positive) new'++falsified, ViewProc rule g1 ws2)
  where
    -- underlying call to stepRule should only return positive MT
    rawFact (MT Positive t) = t { label = toRaw (label t), tval = Truth True }
      where
    falsify :: [Tuple] -> WatchedSet -> ([Msg], WatchedSet)
    falsify ts ws = foldr fix ([], ws) ts
    fix :: Tuple -> ([Msg], WatchedSet) -> ([Msg], WatchedSet)
    fix t (out, ws) = (falsem ++ out, ws2)
      where
        false :: Map Provenance [Tuple]
        false = lookDefault t ws
        falsep = M.keys false
        falsem = map (MT Negative) $ concat $ M.elems false
        ws1 = M.insert t M.empty ws
        ws2 = foldr removeProof ws1 falsep
        --falsem = map (MT Negative) false
        removeProof :: Provenance -> WatchedSet -> WatchedSet
        removeProof p ws = foldr (M.adjust (M.delete p)) ws (matched p)

    indexProof :: Tuple -> WatchedSet -> WatchedSet
    indexProof t w = foldr (\(m, t) -> M.insertWith (M.unionWith (++)) m (M.singleton (source t) [t])) w $ zip (matched $ source t) (repeat t)

showIOM s ms output = unlines $ [s++"input:"] ++ map ppMsg ms ++ ["output:"] ++ map ppMsg output

stepRule :: MsgQueue -> RankedRule -> Graph -> M2 ([Msg], Processor)
stepRule mq@MQ{m_pos = pos, m_neg = neg} rule g = do
    -- remove negative changes
    g1 <- foldM removeTupleM g neg
    -- get new matches
    (g2, output) <- foldM (findMatches rule) (g, []) pos
    return $ tr (showIOM "" (toMsgs mq) output) $
      (output, ObsProc rule g2)

removeTupleM :: Graph -> Tuple -> M2 Graph
removeTupleM g t = return (removeTuple t g)

findMatches rr (g, out) t = do
  new <- concat . map fst <$> mapM applyMatch matches
  let g2 = foldr removeConsumed (insertTuple t g) new
  return (g2, new ++ out)
  where
    matches = getMatches t rr g
    removeConsumed (MT Negative t) g = removeTuple t g
    removeConsumed _ g = g

solve :: [Rule] -> [Msg] -> M2 ()
solve rs msgs = do
    -- logging
    logMsg "solve"
    mapM_ (logMsg . ppMsg) msgs
    -- queue msgs
    commitMsgs msgs
    -- iterate
    unfoldM () (\_ -> withGas (mbool <$> step))
  where
    mbool False = Nothing
    mbool True = Just ()
    unfoldM :: Monad m => a -> (a -> m (Maybe a)) -> m a
    unfoldM s f = do
      m <- f s
      case m of
        Nothing -> return s
        Just s' -> unfoldM s' f
