-- TODO
--  ? detect positive proof cycles; ensure the positive part of the model is always well-founded
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Either (partitionEithers)
import Control.Monad
import Control.Monad.State

import Types
import Rules
import Monad
import Graph (getMatches, applyMatch)

import Debug.Trace
tr _ = id
--tr = trace

splitMap :: (a -> Either b c) -> [a] -> ([b], [c])
splitMap f = partitionEithers . map f

showIOM s ms output = unlines $ [s++"input:"] ++ map ppMsg ms ++ ["output:"] ++ map ppMsg output

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

commitMsgs :: [Msg] -> M2 ()
commitMsgs [] = return ()
commitMsgs msgs = do
  d <- gets db
  let msgs' = filter notRaw msgs
  let db' = foldr updateDB d msgs'
  moddb $ const db'
  mapM_ outputMsg msgs
  pushMsgs msgs

-- TODO no DB parameter?
initPS :: [Rule] -> DB -> PS
initPS rs db = emptyProc
  { dependencies = M.unionWith (++) events rdeps
  , queues = M.empty
  , processors = M.fromList $ processors ++ reducers
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
    reducer v = (AReducer v, emptyReducer v)
    rdeps = M.fromList $ map rdep viewRels
    reducers = map reducer viewRels

    processor rr@(i, rule) =
      case rtype rule of
        Event -> (ARule i, ObsProc  rr graph)
        -- TODO need to populate watched map?
        View  -> (ARule i, ViewProc rr graph M.empty)
    processors = map processor rrs

resetProcessor :: [Rule] -> M2 ()
resetProcessor rs = do
  d <- gets db
  modps (const (initPS rs d))

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

-- Note: a logical relation has three potential values for a given tuple:
--   - true
--   - explicitly false
--   - unobserved
-- Whenever a value changes, a new Positive Msg is emitted, and a Negative Msg
-- is emitted if the tuple had a previous value. An "explicit false" tuple is
-- created whenever a previously true fact becomes false (caused by
-- stepReducer), OR when a negative pattern matches a fact with no proofs yet
-- (see applyMatch).
stepReducer :: MsgQueue -> Label -> RedOp -> ReducedCache -> ReducedValue -> M2 ([Msg], Processor)
stepReducer mq l op state vals = do
  newPairs <- mapM posVal changedf
  let newMsgs = map (MT Positive . snd) newPairs
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
    changedf = filter (\f -> (M.lookup f vals2) /= (tval <$> M.lookup f vals)) $ touchedList
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
        (state1, vals1, S.insert f touched)
      where
        (_,f) = tfact t
        state1 = updateFact p t state
        -- removals handled by final check; see vals2 above
        vals1 = if p == Positive then M.insertWith valOr f (tval t) vals else vals

    updateFact :: Polarity -> Tuple -> ReducedCache -> ReducedCache
    updateFact Positive t = M.insertWith (++) (nodes t) [t]
    updateFact Negative t = M.adjust (filter (/= t)) (nodes t)


updateProc a p = modps $ \ps -> ps { processors = M.insert a p (processors ps) }

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
      where
    -- Remove all proofs depending on ts; return them and the new WS
    falsify :: [Tuple] -> WatchedSet -> ([Tuple], WatchedSet)
    falsify ts ws = foldr fix ([], ws) ts
    fix :: Tuple -> ([Tuple], WatchedSet) -> ([Tuple], WatchedSet)
    fix t (out, ws) = (falsem ++ out, ws2)
      where
        false :: Map Provenance [Tuple]
        false = lookDefault t ws
        falsep = M.keys false
        falsem = concat $ M.elems false
        ws1 = M.insert t M.empty ws
        ws2 = foldr removeProof ws1 falsep
        removeProof :: Provenance -> WatchedSet -> WatchedSet
        removeProof p ws = foldr (M.adjust (M.delete p)) ws (matched p)

    -- Add t to the watched subset of each tuple its proof depends on.
    indexProof :: Tuple -> WatchedSet -> WatchedSet
    indexProof t w = foldr (\(m, t) -> M.insertWith (M.unionWith (++)) m (M.singleton (source t) [t])) w $
      zip (matched $ source t) (repeat t)

stepRule :: MsgQueue -> RankedRule -> Graph -> M2 ([Msg], Processor)
stepRule mq@MQ{m_pos = pos, m_neg = neg} rule g = do
    -- remove negative changes
    g1 <- foldM removeTupleM g neg
    -- get new matches
    (g2, output) <- foldM (findMatches rule) (g1, []) pos
    return $ tr (showIOM (show (fst rule) ++ " ") (toMsgs mq) output) $
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
