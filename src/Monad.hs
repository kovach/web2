{-# LANGUAGE RecordWildCards #-}
module Monad where

import Data.Maybe
import Data.List (delete)
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Types

data DB = DB
  { tuples :: Graph
  , removed_tuples :: [Tuple]
  , node_counter :: Count
  , tuple_counter :: Count
  }

allTuples :: DB -> [Tuple]
allTuples db = fromGraph (tuples db) ++ removed_tuples db

initDB g = DB { tuples = toGraph g
              , removed_tuples = []
              , node_counter = 0, tuple_counter = 1}
emptyDB = initDB []

data MsgQueue = MQ { m_pos :: ![Tuple], m_neg :: ![Tuple] }
instance Monoid MsgQueue where
  mempty = MQ [] []
  -- NOTE this is not good for appending a single message that you know not to
  -- be in the other queue
  mappend (MQ p1 n1) (MQ p2 n2) =
    MQ (foldr delete p1 n2 ++ foldr delete p2 n1)
       (n1 ++ n2)

emptyQueue :: MsgQueue
emptyQueue = mempty
isEmptyQueue (MQ [] []) = True
isEmptyQueue _ = False

toMsgs :: MsgQueue -> [Msg]
toMsgs mq = map (MT Positive) (m_pos mq)
         ++ map (MT Negative) (m_neg mq)

-- TODO give Provenance a unique id
type WatchedSet = IntMap (Map Provenance [Tuple])

type ReducedCache = Map [Node] [Tuple]
type ReducedValue  = Map [Node] Tuple

emptyProcessor r = ObsProc r (toGraph [])
emptyReducer l = Reducer Or l M.empty M.empty

--TODO delete
--data Header = RawHeader Label | Scope Label Header | UnitHeader
--  deriving (Show, Eq, Ord)

data Processor
  -- an *observer* finds matches, produces new tuples
  = ObsProc RankedRule Graph

  -- a *view* wraps an observer with a WatchedSet, which removes a tuple whose proof becomes falsified
  | ViewProc RankedRule Graph WatchedSet

  -- a *reducer* binds "raw" tuples, outputs "reduced" tuples
  -- currently only logical ("or") reduction is supported
  | Reducer RedOp Label ReducedCache ReducedValue

  -- These actors handle interaction between sub-programs
  | CreatorProc Actor
  | WorkerProc
  | SubProgram Actor ProgramName PS
  | JSProc

  -- TODO remove
  -- msgs:
  --  `update-rule rulset-id rule-uid rule-str` ->
  --
  -- program name,
  -- [(Rule, rank, unique id, unparsed string)]
  -- | RuleProc String [(Rule, RuleRank, Node, String)]

  -- `command string`
  --   rule string (.) -> parse (maybe) [Msg] -> physical processor
  --   rule string (!) -> parse (maybe) rule -> add to SubProc
  --   rule string -> parse (maybe) [Msg] -> some reflector -> REPL ui rules
  --   query string -> parse (maybe) [Context], DO ctxt reflection -> REPL ui rules
  --   :load filename -> parse rule file, create SubProc
  --
  -- physical msg -> insert/remove-Tuple
  --
  -- (base Graph)
  -- | REPLEval Graph
  -- | SubProc PS

  -- | ConstProc RHS
  -- -- full program, current status
  -- | SequenceProc [Processor] [Processor]
  -- | Wrap Label Processor


-- add output
data PS = PS
  { dependencies :: Map Label [Actor]
  , queues :: Map Actor MsgQueue
  , sinks :: [Actor]
  , processors :: Map Actor Processor
  , output :: ([Tuple], Set Tuple)
  , ps_name :: String
  -- , gas_limits :: Map Actor Int
  }

emptyPS :: PS
emptyPS = PS mempty mempty mempty mempty mempty "root"

data ReflContext = RC
  { rcf :: Map Fact Node
  , rcp :: Map Provenance Node
  , rcr :: Map RankedRule Node
  , rct :: Map Tuple Node }

emptyReflContext = RC { rcf = M.empty , rcp = M.empty , rcr = M.empty , rct = M.empty }

-- remove new/out_unprocessed, processor
data InterpreterState = IS
  { db :: DB
  -- , processor :: PS
  , new_unprocessed :: [Msg]
  -- , out_unprocessed :: [Msg]
  , msgLog :: [String]
  , gas :: Int
  }

type M2 = State InterpreterState

type RuleId = Node
type ProgramName = String

-- systematic actors communicate through this state
-- They send "normal" [Msg] to each other, but can access Haskell
--   values stored here
data SystemState = SS
  -- map id to parsed Rule
  { rule_map :: Map RuleId (Rule, String)

  -- environment holds dynamically created actors
  -- managed by external PS with two actors:
  --   creator <-> worker
  -- creator can add or reset environment elements
  -- worker accepts messages from creator, `solve`s environment
  , environment :: PS
  , program_map :: Map ProgramName [RuleId]
  , refl_context :: ReflContext
  , tuple_ids :: IntMap Tuple
  , worker_id :: Actor
  }

emptySS = SS M.empty emptyPS M.empty emptyReflContext IM.empty undefined

type SM = StateT SystemState M2

defaultGas = 1000
makeS2 db gas = IS db [] [] gas
emptyIS = makeS2 emptyDB defaultGas

runDB :: Maybe Int -> DB -> M2 a -> (a, InterpreterState)
runDB mgas db m = runState m (makeS2 db (fromMaybe defaultGas mgas))

runStack1 :: SM a -> (a, InterpreterState)
runStack1 m =
  let ((a, _), is) = runStack emptySS emptyIS m
  in (a, is)

runStack :: SystemState -> InterpreterState -> SM a -> ((a, SystemState), InterpreterState)
runStack ss is m = runState (runStateT m ss) is

useGas :: M2 ()
useGas = modify $ \s -> s { gas = gas s - 1}

withGas :: (M2 (Maybe b)) -> M2 (Maybe b)
withGas f = do
  g <- gets gas
  if g < 1 then return Nothing else do
    useGas
    f

withGas2 :: (SM (Maybe b)) -> SM (Maybe b)
withGas2 f = do
  g <- lift $ gets gas
  if g < 1 then return Nothing else do
    lift $ useGas
    f

logMsg :: String -> M2 ()
logMsg m = modify $ \s -> s { msgLog = m : msgLog s }

-- outputMsg :: Msg -> M2 ()
-- outputMsg m = modify $ \s -> s { out_unprocessed = m : out_unprocessed s }

moddb :: (DB -> DB) -> M2 ()
moddb f = modify $ \s -> s { db = f (db s) }

--modps :: (PS -> PS) -> M2 ()
--modps f = modify $ \s -> s { processor = f (processor s) }

freshNode :: M2 Node
freshNode = do
  c <- gets (node_counter . db)
  moddb $ \s -> s { node_counter = c + 1 }
  return (NNode c)

flushEvents :: M2 [Msg]
flushEvents = do
  es <- gets new_unprocessed
  modify $ \s -> s { new_unprocessed = [] }
  return es

-- flushOutput :: M2 [Msg]
-- flushOutput = do
--   es <- gets out_unprocessed
--   modify $ \s -> s { out_unprocessed = [] }
--   return es

packTuple :: RawTuple -> Provenance -> M2 Tuple
packTuple (rel, ns) p = do
  c <- gets (tuple_counter . db)
  moddb $ \s -> s { tuple_counter = c + 1 }
  let t = T { nodes = ns, label = rel, tid = c, tval = NoVal, source = p }
  return t

packTupleVal :: RawTuple -> Provenance -> TVal -> M2 Tuple
packTupleVal f p val = do
  t <- packTuple f p
  return t { tval = val }

storeTuple :: Tuple -> M2 ()
storeTuple t = do
  moddb $ \s -> s { tuples = insertTuple t (tuples s) }

makeTuple :: RawTuple -> Provenance -> M2 Tuple
makeTuple r p = do
  t <- packTuple r p
  storeTuple t
  return t

scheduleAdd :: Tuple -> M2 ()
scheduleAdd t = modify $ \s -> s { new_unprocessed = MT Positive t : new_unprocessed s }

-- NOTE: gives each rule a fresh integer label;
-- They are ordered according to the list.
tagRules :: [Rule] -> M2 [RankedRule]
tagRules rs = mapM fix rs
  where
    fix r = do
      NNode i <- freshNode
      return $ RankedRule i r

--netOutput :: InterpreterState -> [Msg]
--netOutput = removeOpposites . out_unprocessed

-- TODO delete; msg queue for output processor will handle this
--
-- TODO check this
--   esp proofs
--removeOpposites ms = fix s ++ proofs
--  where
--    tuples = ms
--    proofs = []
--    s = S.fromList tuples
--    --split (MT p t) = Left (MT p t)
--    ----split f@(MF _ _ _) = Right f
--    --(tuples, proofs) = partitionEithers $ map split ms
--    op (MT p t) = MT (neg p) t
--
--    fix s = S.toList $ foldr annihilate (S.fromList tuples) tuples
--
--    annihilate t s | op t `S.member` s = S.delete t (S.delete (op t) s)
--    annihilate _ s = s
