{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Main where

import Types
import FactIndex (emptyFS)
import Monad
import Graph
import Update
import Convert

import Data.Set (Set)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import qualified Data.ByteString.Lazy as T (ByteString, pack, unpack, intercalate, append)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)

import BroadcastServer

import Debug.Trace

import Data.Aeson
import GHC.Generics

data Command = Reset
             | Hover {ref :: Node} | UnHover {ref :: Node}
             | Click {button :: Node, ref :: Node}
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

parseCommand (Hover n) = ("hover", [n])
parseCommand (UnHover n) = ("unhover", [n])
parseCommand (Click {button, ref}) = ("click", [ref, button])

convert :: Event -> Maybe (Polarity, Label, [Node])
convert (E p T{..}) = Just (p, label, nodes)
convert (EFact (label, nodes) _) = Just (Positive, label, nodes)
convert (EFalse (label, nodes))  = Just (Negative, label, nodes)

encodeEvents :: [Event] -> T.ByteString
encodeEvents = encode . mapMaybe convert

data State = State [Rule] DB

initGoProgram = do
  (edgeBlocks, rules, _) <- loadProgram "go.graph" "go.arrow"
  uiRules <- readRules "ui/go.arrow"
  let allRules = rules ++ uiRules
  let (_, _, db1, msgs, _) = runProgramWithDB edgeBlocks allRules
  return (allRules, db1, msgs)

handler msg s0@(State rules db0) =
  case decodeCommand msg of
    Just Reset -> do
      (rules', db1, msgs) <- initGoProgram
      -- TODO rename step2
      let (_, outputEvents) = step2 msgs emptyFS
      putStrLn "got reset"
      return (Just (encodeEvents outputEvents), (State rules' db1))
    Just c -> do
      putStrLn "got event"
      let
        (_, is) = runDB Nothing db0 $ do
          t <- packTuple (parseCommand c) nullProv
          let msg = MT Positive t
          solve rules [msg]
        outputMsgs = netOutput is
        (_, outputEvents) = step2 outputMsgs (facts db0)
        db1 = db is
      return (Just (encodeEvents outputEvents), State rules db1)
    Nothing -> do
      putStrLn "decode failed"
      return (Nothing, s0)

main = do
  let rules = []
      db = emptyDB
  runServer (State rules db) handler
