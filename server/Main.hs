{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Main where

import Types
import FactIndex (emptyFS)
import Monad
import Update
import Convert

import BroadcastServer

import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Lazy as T (ByteString)


import Data.Aeson
import GHC.Generics

data Command = Reset | Connect
             | Hover {ref :: Node} | UnHover {ref :: Node}
             | Click {ref :: Node, button :: Node}
             | Key {ref :: Node, key :: Node}
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
instance FromJSON Node where
instance FromJSON Command where

decodeCommand = decode

parseCommand id (Hover n) = ("hover", [NInt id, n])
parseCommand id (UnHover n) = ("unhover", [NInt id, n])
parseCommand id (Click {ref, button}) = ("click", [NInt id, ref, button])
parseCommand id (Key {ref, key}) = ("key-press", [key, NInt id, ref])
parseCommand _ Reset = error "unimplemented"
parseCommand _ Connect = error "unimplemented"

convert :: Event -> Maybe (Polarity, Label, [Node])
convert (E p T{..}) = Just (p, label, nodes)
convert (EFact (label, nodes) _) = Just (Positive, label, nodes)
convert (EFalse (label, nodes))  = Just (Negative, label, nodes)

encodeEvents :: [Event] -> T.ByteString
encodeEvents = encode . mapMaybe convert

data State = State [Rule] DB

initGoProgram :: IO ([Rule], DB, [Msg])
initGoProgram = do
  (edgeBlocks, rules, _) <- loadProgram "server/go.graph" "examples/go.arrow"
  uiRules <- readRules "ui/go.arrow"
  let allRules = rules ++ uiRules
  let (_, _, db1, msgs, _) = runProgramWithDB edgeBlocks allRules
  return (allRules, db1, msgs)

init110Program :: IO ([Rule], DB, [Msg])
init110Program = do
  (edgeBlocks, rules, _) <- loadProgram "examples/110.graph" "examples/110.arrow"
  let allRules = rules
  let (_, _, db1, msgs, _) = runProgramWithDB edgeBlocks allRules
  return (allRules, db1, msgs)

initEditorProgram :: IO ([Rule], DB, [Msg])
initEditorProgram = do
  (edgeBlocks, rules, _) <- loadProgram "ui/editor.graph" "ui/editor.arrow"
  let allRules = rules
  let (_, _, db1, msgs, _) = runProgramWithDB edgeBlocks allRules
  return (allRules, db1, msgs)

makeDB = initGoProgram

handler connId msg s0@(State rules db0) =
  case decodeCommand msg of
    Just Reset -> do
      (rules', db1, msgs) <- makeDB
      -- TODO rename step2
      let (_, outputEvents) = step2 msgs emptyFS
      --mapM_ (putStrLn . ppEvent) outputEvents
      putStrLn "reset"
      return (Just (encodeEvents outputEvents), (State rules' db1))
    Just c -> do
      putStrLn "parsed event"
      let
        (_, is) = runDB Nothing db0 $ do
          --let msg = MT Positive t
          let fact = parseCommand connId c
          msg <- case c of
                  Hover _ -> return $ MF Positive fact (Extern [])
                  UnHover _ -> return $ MF Negative fact (Extern [])
                  Click _ _ -> do
                    t <- packTuple fact nullProv
                    return $ MT Positive t
                  Key _ _ -> do
                    t <- packTuple fact nullProv
                    return $ MT Positive t
                  _ -> error "unhandled"
          solve rules [msg]
        outputMsgs = netOutput is

        (_, outputEvents) = step2 outputMsgs (facts db0)
        db1 = db is
      --mapM_ (putStrLn . ppEvent) outputEvents
      return (Just (encodeEvents outputEvents), State rules db1)
    Nothing -> do
      putStrLn "decode failed"
      return (Nothing, s0)

main = do
  let rules = []
      db = emptyDB
  putStrLn "server starting"
  runServer (State rules db) handler
