-- TODO
--  ? detect positive proof cycles; ensure the positive part of the model is always well-founded
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.Maybe (mapMaybe)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad
import Control.Monad.State

import Types
import Rules
import Monad
import Graph (getMatches, applyMatch)

import Debug.Trace
tr _ = id
--tr = trace

showIOM s ms output = unlines $ [s++"input:"] ++ map ppMsg ms ++ ["output:"] ++ map ppMsg output

-- assumes no queue sees +t after -t
pushQueue :: Msg -> MsgQueue -> MsgQueue
pushQueue (MT Positive t) m@(MQ{m_pos}) = m { m_pos = t : m_pos }
pushQueue (MT Negative t) MQ {m_pos, m_neg} =
  MQ { m_neg = t : m_neg, m_pos = delete t m_pos}

pushMsg :: Msg -> PS -> PS
--pushMsg m@(MT p t) ps = ps { queues = queues'}
pushMsg m@(MT p t) ps = ps { queues = queues', output = pushOutput m (output ps) }
  where
    queues' = foldr (adjustDefault $ pushQueue m) (queues ps) actors
    actors = lookDefault (label t) (dependencies ps)
    pushOutput (MT Positive t) (n, s) = (n, S.insert t s)
    pushOutput (MT Negative t) (n, s) = (t:n, S.delete t s)

sendMsgs :: [Msg] -> PS -> PS
sendMsgs ms ps = foldl' (flip pushMsg) ps ms

-- updates global DB
updateDB :: Msg -> DB -> DB
updateDB (MT Positive t) db = db { tuples = insertTuple t (tuples db) }
updateDB (MT Negative t) db = db { tuples = removeTuple t (tuples db)
                                 , removed_tuples = t : removed_tuples db }

-- getOutputs :: PS -> [Msg]
-- getOutputs = toMsgs . lookDefault Output . queues

--TODO delete
--pushMsgs :: [Msg] -> M2 ()
--pushMsgs = mapM_ (modps . pushMsg)

-- commitMsgs :: [Msg] -> PS -> M2 PS
-- commitMsgs [] ps = return ps
-- commitMsgs msgs ps = do
--   d <- gets db
--   let msgs' = filter notRaw msgs
--   let db' = foldr updateDB d msgs'
--   moddb $ const db'
--   mapM_ outputMsg msgs
--   return $ foldr pushMsg ps msgs

initPS :: [Rule] -> Graph -> M2 PS
initPS rs graph = do
  let rrs = rankRules rs
  let events = step1 rrs
  let processors = map processor rrs
  return PS
    { dependencies = M.unionsWith (++) [events, rdeps]
    , queues = mempty
    , output = mempty
    -- , gas_limits = M.empty
    , processors = M.fromList $ processors ++ reducers }
  where
    step0 :: RankedRule -> Map Label [Actor]
    step0 r@(RankedRule i rule) = M.fromList $ zip (lhsRels rule) (repeat [ActorRule i])

    step1 :: [RankedRule] -> Map Label [Actor]
    step1 = foldr (M.unionWith (++)) M.empty . map step0

    viewRels = logicalRelations rs

    rdep v = (toRaw v, [ActorReducer v])
    reducer v = (ActorReducer v, emptyReducer v)
    reducers = map reducer viewRels

    rdeps = M.fromList $ map rdep viewRels
    -- forward all (non-raw) messages to the "output" queue
    --outputs = M.fromList $ zip (allRelations rs) (repeat [Output])

    processor rr@(RankedRule i rule) =
      case rtype rule of
        Event -> (ActorRule i, ObsProc  rr graph)
        -- TODO need to populate watched map?
        View  -> (ActorRule i, ViewProc rr graph IM.empty)

--TODO delete
--resetProcessor :: [Rule] -> M2 ()
--resetProcessor rs = do
--  d <- gets db
--  modps (const (initPS rs d))

-- Note: a logical relation has three potential values for a given tuple:
--   - explicitly true
--   - explicitly false
--   - unobserved (no value)
-- Whenever a value changes, a new Positive Msg is emitted, and a Negative Msg
-- is emitted if the tuple had a previous value. An "explicit false" tuple is
-- created whenever a previously true fact becomes false (caused by
-- stepReducer), OR when a negative pattern matches a fact with no proofs yet
-- (see applyMatch).
stepReducer :: MsgQueue -> Label -> RedOp -> ReducedCache -> ReducedValue -> M2 ([Msg], Processor)
stepReducer mq l op state vals = do
  newPairs <- mapM posVal changedf
  let newMsgs = map (MPos . snd) newPairs
      valsOut = foldr (uncurry M.insert) vals newPairs
      outputs = mapMaybe negVal changedf ++ newMsgs
      proc = Reducer op l state1 valsOut
  tr (showIOM "reducer " ms outputs) $ return (outputs, proc)
  -- TODO generalize for other folds
  where
    ms = toMsgs mq

    -- compute (new proof lists, new truth values (vals2), and potentially changed facts)
    (state1, vals1, touched) = foldr (step Or) (state, fmap tval vals, S.empty) ms
    touchedList = S.toList touched
    vals2 = foldr (M.adjustWithKey fixIfFalse) vals1 touchedList
      where
        fixIfFalse f v@(Truth True) = if all isFalse (look f state1) then Truth False else v
        fixIfFalse _ v = v
        isFalse t = Truth False == tval t

    -- these facts have new values
    changedf =
      {-# SCC changedf #-}
      filter (\f -> (M.lookup f vals2) /= (tval <$> M.lookup f vals)) $ touchedList
    -- Create a Negative Msg
    negVal f = case M.lookup f vals of
                 Just t -> Just $ MT Negative t
                 _ -> Nothing
    -- Create a Positive Msg
    posVal f = do
        t <- packTupleVal (l, f) p (look f vals2)
        return (f, t)
      where
        p = Reduction op (look f state1)

    valOr (Truth a) (Truth b) = Truth (a || b)

    --  Main fold function
    step Or (MT p t) (state, vals, touched) =
        {-# SCC step #-}
        (state1, vals1, S.insert f touched)
      where
        (_,f) = tfact t
        state1 = updateFact p t state
        -- removals handled by final check; see vals2 above
        vals1 = if p == Positive then M.insertWith valOr f (tval t) vals else vals

    updateFact :: Polarity -> Tuple -> ReducedCache -> ReducedCache
    updateFact Positive t = M.insertWith (++) (nodes t) [t]
    updateFact Negative t = M.adjust (delete t) (nodes t)
    --updateFact Negative t = M.adjust (filter (/= t)) (nodes t)

stepView :: MsgQueue -> RankedRule -> Graph -> WatchedSet -> M2 ([Msg], Processor)
stepView ms@(MQ {m_neg = neg}) rule g ws = do
    (new, ObsProc _ g1) <- stepRule ms rule g
    let (new', falseProps) = splitMap rawFact new
    let (falsified, ws1) = falsify neg ws
        ws2 = foldr indexProof ws1 new'
        fMsgs = map (MT Negative) falsified
        output = map (MT Positive) (new' ++ falseProps) ++ fMsgs
    tr (unlines $ "deletions: " : (map ppMsg fMsgs)) $ return (output, ViewProc rule g1 ws2)
  where
    -- Underlying call to stepRule should only return positive MT
    --   these are tuples created by some rule head
    rawFact (MT Positive t@T{tval = NoVal}) = Left t { label = toRaw (label t), tval = Truth True }
    --   these are negative tuples suggested by some rule body
    rawFact (MT Positive t) = Right t { label = toRaw (label t) }
    --   error
    rawFact (MT Negative t) = error $ "internal error: view processor attached to linear rule! consumed tuple:\n" ++ ppTuple t
      where
    -- Remove all proofs depending on ts; return them and the new WS
    falsify :: [Tuple] -> WatchedSet -> ([Tuple], WatchedSet)
    falsify ts ws = foldr fix ([], ws) ts
    fix :: Tuple -> ([Tuple], WatchedSet) -> ([Tuple], WatchedSet)
    fix t (out, ws) = (falsem ++ out, ws2)
      where
        false :: Map Provenance [Tuple]
        false = ilookDefault (tid t) ws
        falsep = M.keys false
        falsem = concat $ M.elems false
        ws1 = IM.insert (tid t) M.empty ws
        ws2 = foldr removeProof ws1 falsep
        removeProof :: Provenance -> WatchedSet -> WatchedSet
        removeProof p ws = foldr (IM.adjust (M.delete p)) ws (map tid $ matched p)

    -- Add t to the watched subset of each tuple its proof depends on.
    indexProof :: Tuple -> WatchedSet -> WatchedSet
    indexProof t w = foldr (\(m, t) -> IM.insertWith (M.unionWith (++)) (tid m) (M.singleton (source t) [t])) w $
      zip (matched $ source t) (repeat t)

stepRule :: MsgQueue -> RankedRule -> Graph -> M2 ([Msg], Processor)
stepRule mq@MQ{m_pos = pos, m_neg = neg} rule g = do
    -- remove negative changes
    let g1 = foldr removeTuple g neg
    -- get new matches
    (g2, output) <- foldM (findMatches rule) (g1, []) pos
    return $ tr (showIOM (show (ranked_id rule) ++ " ") (toMsgs mq) output) $
      (output, ObsProc rule g2)
  where
    findMatches rr (g, out) t = do
      new <- concat . map fst <$> mapM applyMatch matches
      let g2 = foldr removeConsumed (insertTuple t g) new
      return (g2, new ++ out)
      where
        matches = getMatches t rr g
        removeConsumed (MT Negative t) g = removeTuple t g
        removeConsumed _ g = g

takeQueue :: PS -> Maybe (PS, Actor, MsgQueue)
takeQueue p =
  let work = M.filter (not . isEmptyQueue) (queues p) in
  if M.size work == 0 then Nothing else
    let (k, v) = M.elemAt 0 work
        proc = p { queues = M.insert k emptyQueue work }
    in Just (proc, k, v)

--commitMsgs mq = do
--  let msgs = toMsgs mq
--  moddb $ \d -> foldr updateDB d msgs
--  return (msgs, OutputProc)
--  --mapM_ outputMsg msgs
--  return ([], OutputProc)

step :: PS -> SM (Maybe PS)
step ps = do
    let mwork = takeQueue ps
    case mwork of
      Nothing -> return Nothing
      Just (ps', act, msgs) -> do
        let pr = look act (processors ps')
        (output, pr') <- case pr of
          Reducer op l s vals -> lift $ stepReducer msgs l op s vals
          ObsProc r g -> lift $ stepRule msgs r g
          ViewProc r g watched -> lift $ stepView msgs r g watched
        let pr1 = ps' { processors = M.insert act pr' (processors ps') }
        return $ Just $ sendMsgs output pr1

solve :: [Msg] -> PS -> SM ([Msg], PS)
solve msgs proc = do
    -- logging
    lift $ do
      logMsg "solve"
      mapM_ (logMsg . ppMsg) msgs
    -- queue msgs
    let proc1 = sendMsgs msgs proc
    -- iterate
    proc2 <- unfoldM proc1 (\ps -> withGas2 (step ps))
    let (removed, addedSet) = output proc2
    let outputs = toMsgs MQ { m_pos = S.toList addedSet, m_neg = removed }
    let msgs' = filter notRaw outputs
    lift $ moddb $ \db -> foldr updateDB db msgs'
    return (msgs', proc2)
  where
    unfoldM :: Monad m => a -> (a -> m (Maybe a)) -> m a
    unfoldM s f = do
      m <- f s
      case m of
        Nothing -> return s
        Just s' -> unfoldM s' f
