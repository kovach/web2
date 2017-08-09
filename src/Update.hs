-- TODO
--  ? detect positive proof cycles; ensure the positive part of the model is always well-founded
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Update where

import Data.Maybe (mapMaybe, fromJust)
import Data.List (delete, foldl')
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
import Reflection
import REPL

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

pushMsg :: Msg -> PS -> PS
pushMsg (MNotActor act m') ps = pushMsgTo m' actors ps
  where
    actors = S.toList . S.delete act . S.fromList $ (sinks ps) ++ concat (M.elems (dependencies ps))
pushMsg (MActor act m') ps = pushMsgTo m' [act] ps
pushMsg m ps = pushMsgTo m actors ps
  where
    actors = sinks ps ++ lookDefault (mlabel m) (dependencies ps)

pushMsgTo m actors ps = ps { queues = queues', output = pushOutput m (output ps) }
  where
    queues' = foldr (adjustDefault $ pushQueue m) (queues ps) actors
    pushOutput m s | not (notRaw m) = s
    pushOutput (MT Positive t) (n, s) = (n, S.insert t s)
    pushOutput (MT Negative t) (n, s) = (t:n, S.delete t s)

sendMsgs :: [Msg] -> PS -> PS
-- this foldl' very important
sendMsgs ms ps = foldl' (flip pushMsg) ps ms

-- updates global DB
updateDB :: Msg -> DB -> DB
updateDB (MT Positive t) db = db { tuples = insertTuple t (tuples db) }
updateDB (MT Negative t) db = db { tuples = removeTuple t (tuples db)
                                 , removed_tuples = t : removed_tuples db }

updatePS :: [Rule] -> PS -> PS
updatePS rs ps = foldr fix ps rules
  where
    converted = rankRules $ convertRules (zip [1..] rs)
    rules = converted
    fix (RankedRule _ Rule { rule_id = Nothing }) _ = error "missing rule id"
    fix rrule ps@PS{..} = 
        ps { dependencies = dependencies2
           , processors = M.insert actor proc' processors }
      where
        actor = (ActorRule (ranked_id rrule))
        proc' = case look actor processors of
                  ObsProc _ graph -> ObsProc rrule graph
                  ViewProc _ graph ws -> ViewProc rrule graph ws

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
    , processors = M.fromList $ processors ++ reducers }
  where
    step0 :: RankedRule -> Map Label [Actor]
    step0 r@(RankedRule i rule) = M.fromList $ zip (lhsRels rule) (repeat [ActorRule i])

    step1 :: [RankedRule] -> Map Label [Actor]
    step1 = foldr (M.unionWith (++)) M.empty . map step0

    rsConverted = convertRules (zip [1..] rs)
    rrs = rankRules rsConverted
    events = step1 rrs
    processors = map processor rrs

    unitRules = map (ActorRule . ranked_id) $
      filter (null . lhs . ranked_rule) rrs
    unitQueues = M.fromList $ zip unitRules (repeat $ pushQueue unitMsg mempty)

    viewRels = logicalRelations rsConverted

    rdep v = (toRaw v, [ActorReducer v])
    reducer v = (ActorReducer v, emptyReducer v)
    reducers = map reducer viewRels

    rdeps = M.fromList $ map rdep viewRels

    processor rr@(RankedRule i rule) =
      case rtype rule of
        Event -> (ActorRule i, ObsProc  rr graph)
        -- TODO need to populate watched map?
        View  -> (ActorRule i, ViewProc rr graph IM.empty)

initMetaPS :: [(ProgramName, String)] -> SM PS
initMetaPS ruleSets = do
    worker  <- freshActor
    creator <- freshActor

    let metaDeps = zip commandRelations (repeat [creator])

    -- sets the program_map
    mapM (uncurry newProgram) ruleSets

    --rankedRuleSets <- mapM (secondM rankRules) ruleSets
    --(ruleSetPairs, rc) <- runReflection (mapM (secondM fix) rankedRuleSets) emptyReflContext
    --let ruleMap = M.fromList ruleSetPairs
    -- store in state with rc

    let procs = [ (worker, WorkerProc) , (creator, CreatorProc worker) ]

    modify $ \ss -> ss { worker_id = worker }

    return (PS
      { dependencies = M.fromList $ metaDeps
      , queues = mempty -- M.fromList [(worker, pushQueue unitMsg mempty)]
      , sinks = []
      , processors = M.fromList procs
      , ps_name = "meta-ps"
      , output = mempty
      })
  where
    fix rs = do
      i <- flattenRules rs
      return rs

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
stepRule mq@MQ{m_pos = pos, m_neg = neg} rule g = tr "stepRule" $ do
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

stepWorker :: MsgQueue -> SM ([Msg], Processor)
stepWorker mq@MQ{m_pos, m_neg} = tr ("stepWorker" ++ unlines (map ppMsg (toMsgs mq))) $ do
    envPS <- gets environment
    tupleIds0 <- gets tuple_ids
    allTuples0 <- gets all_tuples
    (msgs, env') <- solve (toMsgs mq) envPS
    setEnv env'
    -- keep tuples up to date
    --let tupleIds1 = foldr (IM.delete . tid) tupleIds0 m_neg
    let posMsgs = m_pos ++ (mapMaybe mpos msgs)
    let tupleIds2 = foldr (\t -> IM.insert (tid t) t) tupleIds0 posMsgs
    let allTuples1 = foldr S.insert allTuples0 posMsgs
    modify $ \ss -> ss { tuple_ids = tupleIds2, all_tuples = allTuples1 }
    return (msgs, WorkerProc)
  -- where
  --   commands = mapMaybe (\t -> (t,) <$> parseWorkerCommand t) m_pos

stepCreator :: MsgQueue -> Actor -> SM ([Msg], Processor)
stepCreator mq@MQ{m_pos, m_neg} worker = tr "stepCreator" $ do
    SS{..} <- get
    output <- concat <$> mapM handleCommand commands
    return (map (MActor worker) output, CreatorProc worker)
  where
    commands = mapMaybe (\t -> (t,) <$> parseMetaCommand t) m_pos

    cause t = Provenance (RankedRule 0 (Rule Nothing Event [] [])) (Just t) [t] []

    reflected t target rootTID =
      lift $ MT Positive <$> packTuple (LA "reflected" 2, [rootTID, target]) (cause t)

    handleCommand (t, DoReflect tid target) = do
      -- TODO correct?
      -- _ <- lift flushEvents
      rc <- gets refl_context
      tlookup <- gets tuple_ids
      let tuple = (fromJust . flip IM.lookup tlookup) tid
      -- TODO better way to get these out
      (rootTID, rc') <- lift $ runReflection (flattenTuple tuple) rc
      modify $ \ss -> ss { refl_context = rc' }
      reflectionMsgs <- lift flushEvents
      --marker <- lift $ MT Positive <$> packTuple (LA "reflected" 2, [rootTID, target]) (cause t)
      marker <- reflected t target rootTID
      return $ marker : reflectionMsgs

    handleCommand (t, MakeApp node name) =
      newProgramProc (cause t) node name

    handleCommand (t, Attributes node target) = do
      allTuples <- gets all_tuples
      rc <- gets refl_context
      let ts = S.toList $ S.filter (\t -> node `elem` nodes t) allTuples
      (roots, rc') <- lift $ runReflection (mapM flattenTuple ts) rc
      modify $ \ss -> ss { refl_context = rc' }
      reflectionMsgs <- lift flushEvents
      ms <- mapM (reflected t target) roots
      -- return ms
      return (ms ++ reflectionMsgs)

    handleCommand (t, EditRule node) = do
      (_, str) <- gets (look node . rule_map)
      msg <- lift $ MT Positive <$> packTuple (LA "rule-string" 2, [NString str, node]) (cause t)
      return [msg]

    handleCommand (t, ChangeRule node str) =
      case replParse str of
        Right (ARule parsed) -> do
          modify $ \ss -> ss { rule_map = M.insert node (parsed, str) (rule_map ss) }
          programName <- findContainingProgram node
          rules <- getProgramRules programName
          updateRunningSubProgram programName rules
          return []
        other -> error $ show other

    -- TODO should just pass
    handleCommand (t, _) = error $ "command unimplemented: " ++ ppTuple t

getProgramRules :: ProgramName -> SM [Rule]
getProgramRules name = do
  SS{program_map, rule_map} <- get
  let ruleIds = look name program_map
      rules = map (\i -> (fst $ look i rule_map){rule_id = Just i}) ruleIds
  return rules

findContainingProgram :: Node -> SM ProgramName
findContainingProgram node = do
  pm <- gets program_map
  case M.toList (M.filter (node `elem`) pm) of
    [(name, _)] -> return name
    _ -> error "TODO: rule used in multiple programs; editing not supported."

updateRunningSubProgram :: ProgramName -> [Rule] -> SM ()
updateRunningSubProgram name rules = modify $ \ss -> ss {
    environment = (environment ss) { processors = fmap fix (processors (environment ss)) } }
  where
    fix :: Processor -> Processor
    fix (SubProgram act name' ps) | name' == name =
      SubProgram act name' (updatePS rules ps)
    fix proc = proc

newProgramProc :: Provenance -> Node -> ProgramName -> SM [Msg]
newProgramProc cause node name = do
  rules <- getProgramRules name
  let actor = ActorObject node
  proc <- SubProgram actor name <$> lift (initPS name rules emptyGraph)
  --let inputs = M.fromList $ zip watches (repeat [actor])
  let fix ps@PS{..} = ps
          { --dependencies = M.unionWith (++) dependencies inputs
           processors = M.insert actor proc processors
          , sinks = actor : sinks
          }
  modify $ \ss -> ss { environment = fix (environment ss) }

  m <- MT Positive <$> lift (packTuple (LA "new" 1, [node]) cause)

  return [m]

takeQueue :: PS -> Maybe (PS, Actor, MsgQueue)
takeQueue p =
  let work = M.filter (not . isEmptyQueue) (queues p) in
  if M.size work == 0 then Nothing else
    let (k, v) = M.elemAt 0 work
        proc = p { queues = M.insert k emptyQueue work }
    in Just (proc, k, v)

step :: PS -> SM (Maybe PS)
step ps = tr ("step: " ++ ps_name ps) $ do
    let mwork = takeQueue ps
    case mwork of
      Nothing -> tr (ps_name ps ++ " done") $ return Nothing
      Just (ps', act, msgs) -> do
        let pr = look act (processors ps')
        (output, pr') <- case pr of
          Reducer op l s vals -> lift $ stepReducer msgs l op s vals
          ObsProc r g -> lift $ stepRule msgs r g
          ViewProc r g watched -> lift $ stepView msgs r g watched
          CreatorProc worker -> stepCreator msgs worker
          WorkerProc -> stepWorker msgs
          SubProgram me name ps -> do
            (out, ps') <- solve (toMsgs msgs) ps
            -- TODO fix this
            return (map (MNotActor me) out, SubProgram me name ps')
        let pr1 = ps' { processors = M.insert act pr' (processors ps') }
        return $ Just $ sendMsgs output pr1

solve :: [Msg] -> PS -> SM ([Msg], PS)
solve msgs proc = tr ("solve: " ++ ps_name proc ++ "\n") $ do
    -- logging
    lift $ do
      logMsg "solve"
      mapM_ (logMsg . ppMsg) msgs
    -- queue msgs
    let proc1 = (sendMsgs msgs proc) { output = mempty } -- don't return input messages as output
    -- iterate
    proc2 <- unfoldM proc1 (\ps -> withGas2 (step ps))
    -- compute outputs
    let (removed, addedSet) = output proc2
    let outputs = toMsgs MQ { m_pos = S.toList addedSet, m_neg = removed }
    lift $ moddb $ \db -> foldr updateDB db outputs
    return (outputs, proc2 { output = mempty })
  where
    unfoldM :: Monad m => a -> (a -> m (Maybe a)) -> m a
    unfoldM s f = do
      m <- f s
      case m of
        Nothing -> return s
        Just s' -> unfoldM s' f

-- TODO RELOCATE THIS
--
-- TODO refactor this into Graph?
-- step2 :: Rule -> Tuple -> (Graph, [Context]) -> (Graph, [Context])
-- step2 rule t (g, out) = (g2, cs ++ out)
--   where
--     matches = map fix $ getMatches t (unsafeRanked 0 rule) g
--     cs :: [Context]
--     cs = map snd matches
--     removed = concatMap fst matches
--     g2 = foldr removeTuple (insertTuple t g) removed
--     fix (p,c,_) = (consumed p, c)
-- 
-- eval :: Action -> Graph -> M2 Out
-- eval (ARule rule) g = do
--   NNode rid <- freshNode
--   let ts = step1 [rule] g
--   (output, _) <- stepRule (MQ { m_pos = ts, m_neg = [] }) (RankedRule rid rule) g
--   return (ORule output)
-- eval (AQuery q) g = do
--   let rule = Rule Event q [] -- fake rule
--   let ts = step1 [rule] g
--       (_, cs) = foldr (step2 rule) (g, []) ts
--   return (OQuery cs)
