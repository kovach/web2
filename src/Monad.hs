{-# LANGUAGE RecordWildCards #-}
module Monad where

import Data.List
import Data.Maybe
import Data.String
import Control.Monad
import Control.Monad.State

import Types

data S2 = S2
  { db :: DB
  , a_unprocessed :: [Tuple]
  , d_unprocessed :: [Tuple]
  , a_buffer :: [Tuple]
  , d_buffer :: [Tuple]
  , index :: Index
  , gas :: Int
  } deriving (Eq, Show, Ord)

type M2 = State S2

defaultGas = 150
emptyS2 = S2 emptyDB [] [] [] [] emptyIndex defaultGas
makeS2 db index = S2 db [] [] [] [] index defaultGas

runDB db index m = runState m (makeS2 db index)

-- Utilities
moddb :: (DB -> DB) -> M2 ()
moddb f = modify $ \s -> s { db = f (db s) }

freshNode :: M2 Node
freshNode = do
  c <- gets (node_counter . db)
  moddb $ \s -> s { node_counter = c + 1 }
  return (NTRef c)

flush :: M2 ([Tuple], [Tuple])
flush = do
  S2{..} <- get
  modify $ \s -> s { a_buffer = [], d_buffer = [] }
  return (a_buffer, d_buffer)

makeTuple :: RawTuple -> Maybe Provenance -> M2 Tuple
makeTuple (rel, ns) p = do
  c <- gets (tuple_counter . db)
  moddb $ \s -> s { tuple_counter = c + 1 }
  return $ T { nodes = ns, label = rel, tid = c, source = p }

insertTuple t = do
  modify $ \s -> s { a_buffer = t : a_buffer s }
  moddb $ \s -> s { tuples = t : tuples s }

scheduleAdd :: RawTuple -> Maybe Provenance -> M2 ()
scheduleAdd r p = do
  t <- makeTuple r p
  modify $ \s -> s { a_unprocessed = t : a_unprocessed s }

scheduleDel :: Tuple -> M2 ()
scheduleDel t = modify $ \s -> s { d_unprocessed = t : d_unprocessed s }

processDels = do
  r <- gets d_unprocessed
  let fix = filter (not . (`elem` r))
  modify $ \s -> s { d_unprocessed = [] }
  moddb $ \s -> s { tuples = fix (tuples s), removed_tuples = r ++ removed_tuples s }
  modify $ \s -> s
    { a_buffer = fix $ a_buffer s
    , d_buffer = r ++ d_buffer s
    }

increment = undefined
