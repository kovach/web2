{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Iterate where

import Data.Maybe (mapMaybe, fromJust)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad.State

import Types
import Monad
import Reflection
import REPL
import Update

import Debug.Trace

pushMsg :: ControlMsg -> PS -> PS
pushMsg (CNotActor act m') ps = pushMsgTo m' actors ps
  where
    actors = S.toList . S.delete act . S.fromList $ (sinks ps) ++ concat (M.elems (dependencies ps))
pushMsg (CActor act m') ps = pushMsgTo m' [act] ps
pushMsg (CMsg m) ps = pushMsgTo m actors ps
  where
    actors = sinks ps ++ lookDefault (mlabel m) (dependencies ps)

pushMsgTo m actors ps = ps { queues = queues', output = pushOutput m (output ps) }
  where
    queues' = foldr (adjustDefault $ pushQueue m) (queues ps) actors
    pushOutput m s | not (notRaw m) = s
    pushOutput (MT Positive t) (n, s) = (n, S.insert t s)
    pushOutput (MT Negative t) (n, s) = (t:n, S.delete t s)

sendMsgs :: [ControlMsg] -> PS -> PS
-- this foldl' very important
sendMsgs ms ps = foldl' (flip pushMsg) ps ms

initMetaPS :: [(ProgramName, String)] -> SM PS
initMetaPS ruleSets = do
    -- sets the program_map
    mapM (uncurry newProgram) ruleSets

    worker  <- freshActor
    creator <- freshActor
    modify $ \ss -> ss { worker_id = worker }
    let procs = [ (worker, WorkerProc worker) , (creator, CreatorProc worker) ]
    let metaDeps = zip commandRelations (repeat [creator])

    return (PS
      { dependencies = M.fromList $ metaDeps
      , queues = mempty
      , sinks = []
      , processors = M.fromList procs
      , ps_name = "meta-ps"
      , output = mempty
      })

stepWorker :: Actor -> MsgQueue -> SM ([ControlMsg], MetaProcessor)
stepWorker worker mq@MQ{m_pos, m_neg} = tr ("stepWorker" ++ unlines (map ppMsg (toMsgs mq))) $ do
    envPS <- gets environment
    tupleIds0 <- gets tuple_ids
    allTuples0 <- gets all_tuples
    (msgs, env') <- solve (map CMsg $ toMsgs mq) envPS
    setEnv env'
    -- keep tuples up to date
    --let tupleIds1 = foldr (IM.delete . tid) tupleIds0 m_neg
    let posMsgs = m_pos ++ (mapMaybe mpos msgs)
    let tupleIds2 = foldr (\t -> IM.insert (tid t) t) tupleIds0 posMsgs
    let allTuples1 = foldr S.insert allTuples0 posMsgs
    modify $ \ss -> ss { tuple_ids = tupleIds2, all_tuples = allTuples1 }
    --reflMsgs <- concat <$> mapM reflectOne msgs
    return (map CMsg msgs, WorkerProc worker)
    --return (map (CActor worker) reflMsgs ++ map CMsg msgs, WorkerProc worker)
    --return (msgs, WorkerProc)
  where
    reflectOne :: Msg -> SM [Msg]
    reflectOne (MT Positive t) = do
      rc <- gets refl_context
      (rootTID, rc') <- lift $ runReflection (flattenTuple t) rc
      modify $ \ss -> ss { refl_context = rc' }
      reflectionMsgs <- lift flushEvents
      return $ reflectionMsgs
    reflectOne _ = return []


-- stepCreator handles various "Interpreter API" messages, including
-- reflection, rule parsing, rule set changes
stepCreator :: MsgQueue -> Actor -> SM ([ControlMsg], MetaProcessor)
stepCreator mq@MQ{m_pos, m_neg} worker = tr "stepCreator" $ do
    output <- concat <$> mapM handleCommand commands
    return (map (CActor worker) output, CreatorProc worker)
  where
    commands = mapMaybe (\t -> (t,) <$> parseMetaCommand t) m_pos

    cause t = Provenance (RankedRule 0 (Rule Nothing Nothing Event [] [])) (Just t) [t] []

    tupleMsg f p = lift $ MT Positive <$> packTuple f p

    reflected t target rootTID = tupleMsg (LA "reflected" 2, [rootTID, target]) (cause t)

    handleCommand :: (Tuple, MetaCommand) -> SM [Msg]
    handleCommand (t, DoReflect (Id tid) target) = do
      -- TODO correct?
      -- _ <- lift flushEvents
      rc <- gets refl_context
      tlookup <- gets tuple_ids
      let tuple = fromJust $ IM.lookup tid tlookup
      (rootTID, rc') <- lift $ runReflection (flattenTuple tuple) rc
      modify $ \ss -> ss { refl_context = rc' }
      reflectionMsgs <- lift flushEvents
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
      return (ms ++ reflectionMsgs)

    handleCommand (t, EditRule node) = do
      str <- gets (fromJust . rule_str . look node . rule_map)
      msg <- tupleMsg (LA "rule-string" 2, [NString str, node]) (cause t)
      return [msg]

    handleCommand (t, ChangeRule node str) =
      case replParse str of
        Right (ARule parsed) -> do
          modify $ \ss -> ss { rule_map = M.insert node parsed (rule_map ss) }
          programName <- findContainingProgram node
          rules <- getProgramRules programName
          updateRunningSubProgram programName rules
          return []
        _ -> return []
        -- other -> error $ show other

    -- TODO implement the rest
    handleCommand (t, _) = return [] -- error $ "command unimplemented: " ++ ppTuple t

getProgramRules :: ProgramName -> SM [Rule]
getProgramRules name = do
  SS{program_map, rule_map} <- get
  let ruleIds = look name program_map
      fix i =
        let rule = look i rule_map
        in rule {rule_id = Just i}
      rules = map fix ruleIds
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
    fix :: MetaProcessor -> MetaProcessor
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

stepMeta :: PS -> SM (Maybe PS)
stepMeta ps = tr ("step: " ++ ps_name ps) $ do
    let mwork = takeQueue ps
    case mwork of
      Nothing -> tr (ps_name ps ++ " done") $ return Nothing
      Just (ps', act, msgs) -> do
        let pr = look act (processors ps')
        (output, pr') <- case pr of
          CreatorProc worker -> stepCreator msgs worker
          WorkerProc worker -> stepWorker worker msgs
          SubProgram me name ps -> do
            (out, ps') <- solve (map CMsg $ toMsgs msgs) ps
            -- TODO fix this
            return (map (CNotActor me) out, SubProgram me name ps')
          BaseProc proc -> do
            (out, proc') <- lift (stepProcessor msgs proc)
            return (map CMsg out, BaseProc proc')
        let pr1 = ps' { processors = M.insert act pr' (processors ps') }
        return $ Just $ sendMsgs output pr1

solve :: [ControlMsg] -> PS -> SM ([Msg], PS)
solve msgs proc = tr ("solve: " ++ ps_name proc ++ "\n") $ do
    -- logging
    lift $ do
      logMsg "solve"
      mapM_ (logMsg . ppCMsg) msgs
    -- queue msgs
    let proc1 = (sendMsgs msgs proc) { output = mempty } -- don't return input messages as output
    -- iterate
    proc2 <- unfoldM proc1 (\ps -> withGas2 (stepMeta ps))
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
