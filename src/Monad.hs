{-# LANGUAGE RecordWildCards #-}
module Monad where

import Data.List
import Data.Maybe
import Data.String
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import Types

import Debug.Trace

data InterpreterState = IS
  { db :: DB
  , new_unprocessed :: [Msg]
  , msgLog :: [Msg]
  -- TODO remove these two
  , index :: Index
  , functions :: [Rule]
  , gas :: Int
  } deriving (Eq, Show, Ord)

type M2 = State InterpreterState

defaultGas = 500
emptyS2 = IS emptyDB [] [] emptyIndex [] defaultGas
makeS2 db index fns gas = IS db [] [] index fns gas

runDB mgas db index fns m = runState m (makeS2 db index fns (fromMaybe defaultGas mgas))

useGas :: M2 ()
useGas = modify $ \s -> s { gas = gas s - 1}

withGas :: (M2 (Maybe b)) -> M2 (Maybe b)
withGas f = do
  g <- gets gas
  if g < 1 then return Nothing else do
    useGas
    f

logMsg :: Msg -> M2 ()
logMsg m = modify $ \s -> s { msgLog = m : msgLog s }

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

makeTuple :: RawTuple -> Provenance -> M2 Tuple
makeTuple (rel, ns) p = do
  c <- gets (tuple_counter . db)
  moddb $ \s -> s { tuple_counter = c + 1 }
  let t = T { nodes = ns, label = rel, tid = c, source = p }
  return t

storeTuple t = do
  moddb $ \s -> s { tuples = insertTuple t (tuples s) }

scheduleAdd :: Tuple -> M2 ()
scheduleAdd t = modify $ \s -> s { new_unprocessed = MT Positive t : new_unprocessed s }

scheduleDel :: Tuple -> M2 ()
scheduleDel t = modify $ \s -> s { new_unprocessed = MT Negative t : new_unprocessed s }
