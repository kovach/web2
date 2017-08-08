-- TODO print gas used
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as T (ByteString)
import Data.Aeson
import GHC.Generics
import Control.Monad (foldM)

import Types
import Monad
import Rules
import Update
import Convert
import Reflection
import REPL

import BroadcastServer

import Debug.Trace

data Command = Reset | Connect
             | RawTuple {rawLabel :: Label, rawNodes :: [Node]}
             -- TODO: these should be a logical relation
             -- | Hover {ref :: Node} | UnHover {ref :: Node}
  deriving (Generic, Show)
deriving instance Generic Label
deriving instance Generic Node
deriving instance Generic Polarity
instance ToJSON Label where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Node where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Polarity where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Label where
instance FromJSON Node where
instance FromJSON Command where

decodeCommand = decode

convert :: Msg -> Maybe (Polarity, Label, [Node], Int, Bool)
convert (MT p T{..}) = Just (p, label, nodes, tid, fix tval)
  where
    fix (Truth t) = t
    fix NoVal = True

-- TODO remove arity tags?
encodeEvents :: [Msg] -> T.ByteString
encodeEvents = encode . mapMaybe convert

data State = State PS SystemState InterpreterState

type Init = (PS, DB, [Msg])

--initGoProgram :: IO Init
--initGoProgram = do
--  edgeBlocks <- readDBFile "server/go.graph"
--  rules <- readRules  "examples/go.arrow"
--  uiRules <- readRules "ui/go.arrow"
--  --let allRules = convertRules $ zip [1..] $ rules ++ uiRules
--  let allRules = rules ++ uiRules
--  let ((_, ps), s) = runProgramWithDB edgeBlocks allRules
--      result = db s
--      msgs = netOutput s
--  return (ps, result, msgs)
--
--init110Program :: IO Init
--init110Program = do
--  edgeBlocks <- readDBFile "examples/110.graph"
--  rules <- readRules "examples/110.arrow"
--  let ((_, ps), s) = runProgramWithDB edgeBlocks rules
--      result = db s
--      msgs = netOutput s
--  return (ps, result, msgs)

--initEditorProgram :: IO ([Rule], DB, [Msg])
--initEditorProgram = do
--  editDB    <- readDBFile "ui/editor/editor.graph"
--  editRules <- readRules "ui/editor/editor.arrow"
--  objRules  <- readRules "ui/editor/editor.arrow"
--  --objRules  <- readRules "ui/editor/test.arrow"
--  let prog = do
--        flattenRules (rankRules objRules) -- definitely ok since we aren't computing any effects of these rules
--        ms <- flushEvents
--        programWithDB editDB editRules
--        solve editRules ms
--  let (_, s) = runDB Nothing emptyDB prog
--  --mapM_ (putStrLn . ppTupleProv) (fromGraph . tuples $ db s)
--  return (editRules, db s, netOutput s)

--initLogProgram :: IO ([Rule], DB, [Msg])
--initLogProgram = do
--  --objDB    <- readDBFile "ui/editor/test.graph"
--  --objRules    <- readRules "ui/editor/test.arrow"
--  objDB    <- readDBFile "examples/sieve.graph"
--  objRules    <- readRules "examples/sieve.arrow"
--  logRules  <- readRules "ui/editor/log.arrow"
--  editDB    <- readDBFile "ui/editor/editor.graph"
--  editRules <- readRules "ui/editor/editor.arrow"
--  -- TODO better way to load multiple rule sets
--  let allRules = convertRules $ zip [1..] $ logRules ++ editRules
--  let prog = do
--        programWithDB objDB objRules
--        ms <- flushOutput
--        let fix c (MT _ t) = snd <$> flattenTuple c t
--        foldM fix emptyRC ms
--        ms <- flushEvents
--        programWithDB editDB allRules
--        solve allRules ms
--  let (_, s) = runDB Nothing emptyDB prog
--  --mapM_ (putStrLn . ppTupleProv) (fromGraph . tuples $ db s)
--  putStrLn $ "initial db size: " ++ show (length $ fromGraph . tuples $ db s)
--  return (allRules, db s, netOutput s)

makeDB = do
  let files = [
        --("test1", "examples/test.arrow")
        -- , ("test2", "examples/test2.arrow")
         --("button", "examples/test.arrow")
         --("button", "ui/components/button.arrow")
         ("files", "ui/components/files.arrow")
        ]
  strs <- mapM (readFile . snd) files
  let fix s = do
        n1 <- freshNode
        let mk = LA "make-app" 2
        m1 <- MT Positive <$> (packTuple (mk, [n1, NString s]) (Extern []))
        return m1
  return $ runStack emptySS emptyIS $ do
    ps <- initMetaPS (zip (map fst files) strs)
    ms <- lift $ mapM (fix . fst) files
    -- register rulesets
    (output1, ps1) <- solve ms ps
    return (output1, ps1)

noDebug = False

handler connId msg s0@(State ps ss is) =
  case decodeCommand msg of
    Just Reset -> do
      putStrLn "reset"
      (((msgs, ps'), ss'), is') <- makeDB
      -- TODO only send relevant tuples
      --let (_, outputEvents) = step2 msgs emptyFS
      putStrLn $ "init: " ++ unlines (map ppMsg msgs)
      return (Just (encodeEvents msgs), State ps' ss' is')
    Just Connect -> do
      putStrLn "not implemented"
      return (Nothing, s0)
    Just RawTuple{rawLabel, rawNodes} -> do
      putStrLn "parsed event"
      let
        (((msgs, ps'), ss'), is') = runStack ss is $ do
          -- mark tuple with connection id
          let ns = rawNodes -- NInt connId : rawNodes
              l = LA (lstring rawLabel) (length ns)
          t <- lift $ packTuple (l, ns) (Extern [])
          worker <- gets worker_id
          let msg = (MActor worker (MT Positive t))
          solve [msg] ps

        --outputMsgs = netOutput is
        --outputEvents = filter notRaw outputMsgs

      unless noDebug $ do
        putStrLn "new"
        mapM_ (putStrLn . ppMsg) msgs
      return (Just (encodeEvents msgs), State ps' ss' is')
    Nothing -> do
      putStrLn "decode failed"
      return (Nothing, s0)

main = do
  let rules = []
  putStrLn "server starting"
  runServer (State emptyPS emptySS emptyIS) handler
