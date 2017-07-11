{-# LANGUAGE RecordWildCards #-}
module Monad where

import Data.List
import Data.Maybe
import Data.String
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Either (partitionEithers)

import Types
import FactIndex

import Debug.Trace

data DB = DB
  { tuples :: Graph
  , facts :: FactState
  , removed_tuples :: [Tuple] -- TODO remove?
  , node_counter :: Count
  , tuple_counter :: Count
  }

allTuples :: DB -> [Tuple]
allTuples db = fromGraph (tuples db) ++ removed_tuples db

initDB g = DB { tuples = toGraph g, facts = emptyFS
              , removed_tuples = []
              , node_counter = 0, tuple_counter = 0}
emptyDB = initDB []

data InterpreterState = IS
  { db :: DB
  , new_unprocessed :: [Msg]
  , out_unprocessed :: [Msg]
  -- TODO juse use [String] ?
  , msgLog :: [String]
  , gas :: Int
  }

type M2 = State InterpreterState

defaultGas = 1500
emptyS2 = IS emptyDB [] [] [] defaultGas
makeS2 db gas = IS db [] [] [] gas

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

freshNode :: M2 Node
freshNode = do
  c <- gets (node_counter . db)
  moddb $ \s -> s { node_counter = c + 1 }
  return (NTRef c)

flushEvents :: M2 [Msg]
flushEvents = do
  es <- gets new_unprocessed
  modify $ \s -> s { new_unprocessed = [] }
  return es

--TODO remove
--flushOutput :: M2 [Msg]
--flushOutput = do
--  es <- gets out_unprocessed
--  modify $ \s -> s { out_unprocessed = [] }
--  return es

packTuple :: RawTuple -> Provenance -> M2 Tuple
packTuple (rel, ns) p = do
  c <- gets (tuple_counter . db)
  moddb $ \s -> s { tuple_counter = c + 1 }
  let t = T { nodes = ns, label = rel, tid = c, source = p }
  return t

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
    s = S.fromList tuples
    split (MT p t) = Left (MT p t)
    split f@(MF _ _ _) = Right f
    (tuples, proofs) = partitionEithers $ map split ms
    op (MT p t) = MT (neg p) t

    fix s = S.toList $ foldr annihilate (S.fromList tuples) tuples

    annihilate t s | op t `S.member` s = S.delete t (S.delete (op t) s)
    annihilate _ s = s
