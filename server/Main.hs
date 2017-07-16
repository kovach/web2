-- TODO print gas used
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Main where

import Data.Maybe (mapMaybe)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as T (ByteString)
import Data.Aeson
import GHC.Generics

import Types
import FactIndex (emptyFS)
import Monad
import Update
import Convert
import Reflection

import BroadcastServer

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

convert :: Event -> Maybe (Polarity, Label, [Node])
convert (E p T{..}) = Just (p, label, nodes)
convert (EFact (label, nodes) _) = Just (Positive, label, nodes)
convert (EFalse (label, nodes))  = Just (Negative, label, nodes)

encodeEvents :: [Event] -> T.ByteString
encodeEvents = encode . mapMaybe convert

data State = State [Rule] DB

initGoProgram :: IO ([Rule], DB, [Msg])
initGoProgram = do
  edgeBlocks <- readDBFile "server/go.graph"
  rules <- readRules  "examples/go.arrow"
  uiRules <- readRules "ui/go.arrow"
  let allRules = rules ++ uiRules
  let (_, s) = runProgramWithDB edgeBlocks allRules
      result = db s
      msgs = netOutput s
  return (allRules, result, msgs)

init110Program :: IO ([Rule], DB, [Msg])
init110Program = do
  edgeBlocks <- readDBFile "examples/110.graph"
  rules <- readRules "examples/110.arrow"
  let allRules = rules
  let (_, s) = runProgramWithDB edgeBlocks allRules
      result = db s
      msgs = netOutput s
  return (allRules, result, msgs)

initEditorProgram :: IO ([Rule], DB, [Msg])
initEditorProgram = do
  editDB    <- readDBFile "ui/editor/editor.graph"
  editRules <- readRules "ui/editor/editor.arrow"
  --objRules  <- readRules "ui/editor/editor.arrow"
  objRules  <- readRules "ui/editor/test.arrow"
  metaRules <- readRules "ui/editor/creation.arrow"
  --let analysis  = runAnalysis objRules metaRules
  --    (_, init) = runProgramWithDB editDB editRules
  --mapM_ (putStrLn . ppMsg) (netOutput init)
  let prog = do
        --runAnalysis objRules metaRules
        mapM_ flattenRule objRules
        --ms <- flushOutput
        programWithDB editDB (editRules++metaRules)
        --solve editRules ms
  let (_, s) = runDB Nothing emptyDB prog
  mapM_ (putStrLn . ppTupleProv) (fromGraph . tuples $ db s)
  return (editRules, db s, netOutput s)

makeDB = initEditorProgram

noDebug = True

handler connId msg s0@(State rules db0) =
  case decodeCommand msg of
    Just Reset -> do
      (rules', db1, msgs) <- makeDB
      -- TODO rename step2
      let (_, outputEvents) = step2 msgs emptyFS
      --mapM_ (putStrLn . ppEvent) outputEvents
      putStrLn "reset"
      return (Just (encodeEvents outputEvents), (State rules' db1))
    Just Connect -> error "not implemented"
    Just RawTuple{rawLabel, rawNodes} -> do
      putStrLn "parsed event"
      let
        (_, is) = runDB Nothing db0 $ do
          t <- packTuple (rawLabel, NInt connId : rawNodes) (Extern [])
          let msg = MT Positive t
                  --Hover _ -> return $ MF Positive fact (Extern [])
                  --UnHover _ -> return $ MF Negative fact (Extern [])
          solve rules [msg]
        outputMsgs = netOutput is

        (_, outputEvents) = step2 outputMsgs (facts db0)
        db1 = db is
      unless noDebug $ do
        putStrLn "raw output"
        mapM_ (putStrLn . ppMsg) (out_unprocessed is)
        putStrLn "net"
        mapM_ (putStrLn . ppEvent) outputEvents
      return (Just (encodeEvents outputEvents), State rules db1)
    Nothing -> do
      putStrLn "decode failed"
      return (Nothing, s0)

main = do
  let rules = []
      db = emptyDB
  putStrLn "server starting"
  runServer (State rules db) handler
