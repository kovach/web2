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

import Types
import Monad
import Iterate

import BroadcastServer

import Debug.Trace

data Command = Reset | Connect
             | RawTuple {rawLabel :: Label, rawNodes :: [Node]}
             -- TODO: these should be a logical relation
             -- | Hover {ref :: Node} | UnHover {ref :: Node}
  deriving (Generic, Show)
deriving instance Generic Label
deriving instance Generic Id
deriving instance Generic Node
deriving instance Generic Polarity
instance ToJSON Label where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Node where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Polarity where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Label where
instance FromJSON Id where
instance FromJSON Node where
instance FromJSON Command where

decodeCommand = decode

convert :: Msg -> Maybe (Polarity, Label, [Node], Node, Bool)
convert (MT p T{..}) = Just (p, label, nodes, NNode (Id tid), fix tval)
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

makeDB = do
  let files = [ ("refl", "ui/components/refl.arrow")
              , ("button", "ui/components/button.arrow")
              , ("rules",  "ui/components/rule-set.arrow") ]
  strs <- mapM (readFile . snd) files
  let fix s = do
        n1 <- freshNode
        m1 <- packTuple (LA "make-app" 2, [n1, NString s]) (Extern [])
        return $ CMsg (MT Positive m1)
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
          -- reset gas limit
          lift $ setGas 500
          -- TODO mark tuple with connection id
          let ns = rawNodes
              l = LA (lstring rawLabel) (length ns)
          t <- lift $ packTuple (l, ns) (Extern [])
          worker <- gets worker_id
          let msg = (CActor worker (MT Positive t))
          solve [msg] ps

      unless noDebug $ do
        putStrLn "new"
        mapM_ (putStrLn . ppMsg) msgs
      putStrLn "done"
      return (Just (encodeEvents msgs), State ps' ss' is')
    Nothing -> do
      putStrLn "decode failed"
      return (Nothing, s0)

main = do
  putStrLn "server starting"
  runServer (State emptyPS emptySS emptyIS) handler
