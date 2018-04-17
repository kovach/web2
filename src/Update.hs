-- TODO
--  ? detect positive proof cycles; ensure the positive part of the model is always well-founded
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.Maybe (mapMaybe, fromJust)
import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Types
import Rules
import Monad
import Graph (getMatches, applyMatch)
import Reflection

import Debug.Trace
tr _ = id
--tr = trace

showIOM s ms output = unlines $ [s++"input:"] ++ map ppMsg ms ++ ["output:"] ++ map ppMsg output

-- assumes no queue sees +t after -t
pushQueue :: Msg -> MsgQueue -> MsgQueue
pushQueue m mq = tr ("push msg:\n" ++ ppMsg m) $ pushQueue' m mq
pushQueue' (MT Positive t) m@(MQ{m_pos}) = m { m_pos = t : m_pos }
pushQueue' (MT Negative t) MQ {m_pos, m_neg} =
  MQ { m_neg = t : m_neg, m_pos = delete t m_pos}

-- updates global DB
updateDB :: Msg -> DB -> DB
updateDB (MT Positive t) db = db { tuples = insertTuple t (tuples db) }
updateDB (MT Negative t) db = db { tuples = removeTuple t (tuples db)
                                 , removed_tuples = t : removed_tuples db }

-- TODO report error?
updatePS :: [Rule] -> PS -> PS
updatePS rs ps =
  case converted of
    Right rs -> foldr fix ps rs
    Left err -> ps
  where
    converted = rankRules <$> convertRules (zip [1..] rs)
    fix (RankedRule _ Rule { rule_id = Nothing }) _ = error "missing rule id"
    fix rrule ps@PS{..} = 
        ps { dependencies = dependencies2
           , processors = M.insert actor proc' processors }
      where
        actor = (ActorRule (ranked_id rrule))
        proc' = case look actor processors of
                  BaseProc (ObsProc _ graph) -> BaseProc (ObsProc rrule graph)
                  BaseProc (ViewProc _ graph ws) -> BaseProc (ViewProc rrule graph ws)

        dependencies1 = fmap (filter (/= actor)) dependencies
        dependencies2 = M.unionWith (++) dependencies1
          (M.fromList $ zip (lhsRels $ ranked_rule rrule) (repeat [actor]))

initPS :: ProgramName -> [Rule] -> Graph -> M2 PS
initPS name rs graph = do
  return PS
    { dependencies = M.unionsWith (++) [events, rdeps]
    , queues = unitQueues
    , output = mempty
    , sinks = mempty
    , ps_name = name
    , processors = M.fromList . map (second BaseProc) $ processors ++ reducers }
  where
    step0 :: RankedRule -> Map Label [Actor]
    step0 r@(RankedRule i rule) = M.fromList $ zip (lhsRels rule) (repeat [ActorRule i])

    step1 :: [RankedRule] -> Map Label [Actor]
    step1 = foldr (M.unionWith (++)) M.empty . map step0

    rsConverted = case convertRules (zip [1..] rs) of
      Left err -> error err
      Right rc -> rc
    rrs = rankRules rsConverted
    events = step1 rrs
    processors = map processor rrs

    unitRules = map (ActorRule . ranked_id) $
      filter (null . lhs . ranked_rule) rrs
    unitQueues = M.fromList $ zip unitRules (repeat $ pushQueue unitMsg mempty)

    viewRels = viewRelations rsConverted

    reducedRels = reducedRelations rsConverted

    rdep (v, _) = (toRaw v, [ActorReducer v])
    reducer (v, op) = (ActorReducer v, emptyReducer op v)
    reducers = map reducer reducedRels

    rdeps = M.fromList $ map rdep reducedRels

    processor rr@(RankedRule i rule) =
      case rule_type rule of
        Event -> (ActorRule i, ObsProc  rr graph)
        -- TODO need to populate watched map?
        View  -> (ActorRule i, ViewProc rr graph IM.empty)

-- Main incremental update function. Handles three cases:
--   reduction update
--   view rule update
--   standard rule update
stepProcessor :: MsgQueue -> Processor -> M2 ([Msg], Processor)

-- A logical relation has three potential values for a given tuple:
--   - explicitly true
--   - explicitly false
--   - unobserved (no value)
-- Whenever a value changes, a new Positive Msg is emitted, and a Negative Msg
-- is emitted if the tuple had a previous value. An "explicit false" tuple is
-- created whenever a previously true fact becomes false (caused by
-- stepReducer), OR when a negative pattern matches a fact with no proofs yet
-- (see applyMatch).
stepProcessor mq (Reducer op l state vals) = do
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
    (state1, vals1, touched) = foldr (step op) (state, fmap tval vals, S.empty) ms
    touchedList = S.toList touched
    vals2 = foldr (M.adjustWithKey fixIfFalse) vals1 touchedList
      where
        fixIfFalse f v@(Truth True) = if all isFalse (look f state1) then Truth False else v
        fixIfFalse f v@(TVNode _) = sumtvs (look f state1)
        fixIfFalse _ v = v
        isFalse t = Truth False == tval t
        -- TODO this isn't needed
        sumtvs = TVNode . NInt . sum . map (fromtvs . tval)
        fromtvs (TVNode (NInt i)) = i

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

    foldVal Positive (Truth a) (Truth b) = Truth (a || b)
    foldVal Negative (Truth a) _ = Truth a
    foldVal _ (TVNode (NInt i1)) (TVNode (NInt i2)) = TVNode (NInt $ i1 + i2)

    mneg Positive x = x
    mneg Negative (TVNode (NInt i)) = TVNode $ NInt (-i)
    mneg _ x = x

    --  Main fold function
    step _ (MT p t) (state, vals, touched) =
        {-# SCC step #-}
        (state1, vals1, S.insert f touched)
      where
        (_,f) = tfact t
        state1 = updateFact p t state
        -- removals handled by final check; see vals2 above
        vals1 = M.insertWith (foldVal p) f (mneg p $ tval t) vals

    updateFact :: Polarity -> Tuple -> ReducedCache -> ReducedCache
    updateFact Positive t = M.insertWith (++) (nodes t) [t]
    updateFact Negative t = M.adjust (delete t) (nodes t)

stepProcessor ms@(MQ {m_neg = neg}) (ViewProc rule g ws) = do
    (new, ObsProc _ g1) <- stepProcessor ms (ObsProc rule g)
    let (new', falseProps) = splitMap rawFact new
    let (ws1, falsified) = falsify neg ws
        ws2 = foldr indexProof ws1 new'
        fMsgs  = map (MT Negative) falsified
        output = map (MT Positive) (new' ++ falseProps) ++ fMsgs
    tr (unlines $ "deletions: " : (map ppMsg fMsgs)) $
      return (output, ViewProc rule g1 ws2)
  where
    -- Underlying call to stepRule should only return positive MT
    --   these are negative tuples suggested by some rule body
    -- TODO move toRaw into solveStep?
    rawFact (MT Positive t@T{tval = Truth False}) = Right t { label = toRaw (label t) }
    --   these are tuples created by some rule head
    rawFact (MT Positive t) = Left t
    --   error
    rawFact (MT Negative t) = error $ "internal error: view processor attached to linear rule! consumed tuple:\n" ++ ppTuple t

    -- Remove all proofs depending on ts; return them and the new WS
    falsify :: [Tuple] -> WatchedSet -> (WatchedSet, [Tuple])
    falsify ts ws = runWriter $ foldM falsifyOne ws ts
    falsifyOne :: WatchedSet -> Tuple -> Writer [Tuple] WatchedSet
    falsifyOne ws t = writer (ws2, falseTuples)
      where
        false :: Map Provenance [Tuple]
        false = ilookDefault (tid t) ws
        falseTuples = concat $ M.elems false
        ws1 = IM.insert (tid t) M.empty ws
        ws2 = foldr removeProof ws1 (M.keys false)
        removeProof :: Provenance -> WatchedSet -> WatchedSet
        removeProof p ws = foldr (IM.adjust (M.delete p)) ws (map tid $ matched p)

    -- Add t to the watched subset of each tuple its proof depends on.
    indexProof :: Tuple -> WatchedSet -> WatchedSet
    indexProof t w = foldr (\(m, t) -> IM.insertWith (M.unionWith (++)) (tid m) (M.singleton (source t) [t])) w $
      zip (matched $ source t) (repeat t)

-- Main rule update
stepProcessor mq@MQ{m_pos = pos, m_neg = neg} (ObsProc rule g) = tr "stepRule" $ do
    -- remove negative changes
    let g1 = foldr removeTuple g neg
    -- process new matches resulting from positive tuples
    (g2, output) <- foldM (findMatches rule) (g1, []) pos
    return $ tr (showIOM (show (ranked_id rule) ++ " ") (toMsgs mq) output) $
      (output, ObsProc rule g2)
  where
    -- TODO WriterT
    findMatches rule (g, out) t = do
        -- Combine all new Msgs from matches
        new <- concat . map fst <$> mapM applyMatch matches
        -- Insert current tuple; immediately remove consumed tuples
        let g2 = foldr removeConsumed (insertTuple t g) new
        return (g2, new ++ out)
      where
        -- find matches
        matches = getMatches t rule g
        removeConsumed (MT Negative t) g = removeTuple t g
        removeConsumed _ g = g

-- Determines the next actor to receive control; empties its queue.
takeQueue :: PS -> Maybe (PS, Actor, MsgQueue)
takeQueue p =
  let work = M.filter (not . isEmptyQueue) (queues p) in
  if M.size work == 0 then Nothing else
    let (k, v) = M.elemAt 0 work
        proc = p { queues = M.insert k emptyQueue work }
    in Just (proc, k, v)
