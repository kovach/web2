{-# LANGUAGE RecordWildCards #-}
module Monad where

import Data.Maybe
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Types

data DB = DB
  { tuples :: Graph
  , removed_tuples :: [Tuple] -- TODO remove?
  , node_counter :: Count
  , tuple_counter :: Count
  }

allTuples :: DB -> [Tuple]
allTuples db = fromGraph (tuples db) ++ removed_tuples db

initDB g = DB { tuples = toGraph g
              , removed_tuples = []
              , node_counter = 0, tuple_counter = 0}
emptyDB = initDB []

type WatchedSet = Map Tuple (Map Provenance [Tuple])

type ReducedCache = Map [Node] [Tuple]
type ReducedValue  = Map [Node] Tuple

data Processor
  -- observers find matches, produce new tuples
  = ObsProc RankedRule Graph
  -- views wrap an observer with a WatchedSet, which removes tuples whose proofs become falsified
  | ViewProc RankedRule Graph WatchedSet
  -- reducers bind "raw" tuples, output "reduced" tuples
  -- currently only logical ("or") reduction is supported
  | Reducer RedOp Label ReducedCache ReducedValue

emptyProcessor r = ObsProc r (toGraph [])
emptyReducer l = Reducer Or l M.empty M.empty

data Actor = ActorReducer Label | ActorRule RuleId
  deriving (Eq, Show, Ord)

data RelationType = RelNormal | RelBool
  deriving (Eq, Show, Ord)

data MsgQueue = MQ { m_pos :: [Tuple], m_neg :: [Tuple] }
emptyQueue = MQ [] []
isEmptyQueue (MQ [] []) = True
isEmptyQueue _ = False

toMsgs :: MsgQueue -> [Msg]
toMsgs mq = map (MT Positive) (m_pos mq)
         ++ map (MT Negative) (m_neg mq)

data PS = PS
  { dependencies :: Map Label [Actor]
  , queues :: Map Actor MsgQueue
  , processors :: Map Actor Processor
  }

emptyProc = PS M.empty M.empty M.empty

data InterpreterState = IS
  { db :: DB
  , processor :: PS
  , new_unprocessed :: [Msg]
  , out_unprocessed :: [Msg]
  , msgLog :: [String]
  , gas :: Int
  }

type M2 = State InterpreterState

defaultGas = 1000
makeS2 db gas = IS db emptyProc [] [] [] gas

runDB :: Maybe Int -> DB -> M2 a -> (a, InterpreterState)
runDB mgas db m = runState m (makeS2 db (fromMaybe defaultGas mgas))

useGas :: M2 ()
useGas = modify $ \s -> s { gas = gas s - 1}

withGas :: (M2 (Maybe b)) -> M2 (Maybe b)
withGas f = do
  g <- gets gas
  if g < 1 then return Nothing else do
    useGas
    f

logMsg :: String -> M2 ()
logMsg m = modify $ \s -> s { msgLog = m : msgLog s }

outputMsg :: Msg -> M2 ()
outputMsg m = modify $ \s -> s { out_unprocessed = m : out_unprocessed s }

moddb :: (DB -> DB) -> M2 ()
moddb f = modify $ \s -> s { db = f (db s) }

modps :: (PS -> PS) -> M2 ()
modps f = modify $ \s -> s { processor = f (processor s) }

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

flushOutput :: M2 [Msg]
flushOutput = do
  es <- gets out_unprocessed
  modify $ \s -> s { out_unprocessed = [] }
  return es

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

scheduleDel :: Tuple -> M2 ()
scheduleDel t = modify $ \s -> s { new_unprocessed = MT Negative t : new_unprocessed s }

netOutput :: InterpreterState -> [Msg]
netOutput = removeOpposites . out_unprocessed

-- TODO check this
--   esp proofs
removeOpposites ms = fix s ++ proofs
  where
    tuples = ms
    proofs = []
    s = S.fromList tuples
    --split (MT p t) = Left (MT p t)
    ----split f@(MF _ _ _) = Right f
    --(tuples, proofs) = partitionEithers $ map split ms
    op (MT p t) = MT (neg p) t

    fix s = S.toList $ foldr annihilate (S.fromList tuples) tuples

    annihilate t s | op t `S.member` s = S.delete t (S.delete (op t) s)
    annihilate _ s = s
