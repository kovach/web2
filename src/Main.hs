{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Types
import Expr
import Graph
import Rules
import Parser2
import Parse
import Increment
import Reflection

import qualified Data.ByteString.Lazy as T (pack, unpack, intercalate, append)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Network.TextServer
import GHC.Generics

import Data.List (nub, sortOn)
import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace

import Data.Aeson

deriving instance Generic Label
deriving instance Generic Node

data TupleJSON = TJ
  { t_nodes :: [Node]
  , t_label :: Label
  }
  deriving (Generic, Show)

instance ToJSON Label where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Node where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON TupleJSON where
  toEncoding = genericToEncoding defaultOptions

--tupleJSON :: Tuple -> String
tupleJSON t = (TJ { t_nodes = nodes t, t_label = label t })

bunlines = T.intercalate "\n"

data Command = Reset | Hover Node | UnHover Node | Click Node
  deriving (Generic, Show)
instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Node where
instance FromJSON Command where

parseCommand = decode

parseEvent (Hover n) = ("hover", [n])
parseEvent (UnHover n) = ("unhover", [n])
parseEvent (Click n) = ("click", [n])

encodeDB = encode . reverse . map tupleJSON . tuples

data State = State [Label] [Node] Index DB

validActions _ [] = []
validActions [] as = as
validActions [choice] as = filter (any (== choice) . snd) as
validActions choices as =
  let ps = filter ((\ns -> all (`elem` ns) choices) . snd) as
      correct = filter ((== choices) . snd) as
  in case ps of
       [] -> []
       a:_ -> if length choices == length (snd a) then correct else ps

shared as bs = S.size (S.fromList as `S.intersection` S.fromList bs)

handler msg noop@(State inputs choices ind db) =
  case parseCommand msg of
    Just Reset -> do
      -- Initialize everything
      (_,ind',inputs,db') <- mainUI
      putStrLn "actions:"
      mapM_ print inputs
      putStrLn ""
      return (Just $ encodeDB db , State inputs choices ind' db')
    -- Special handling for click, for now
    Just (Click n) -> do
      let acts0 = map (actions db ind) inputs
          cs = nub $ choices ++ [n]
          acts1 = concatMap (validActions cs) acts0
      putStrLn $ "click: " ++ show n ++ "\nchoices: " ++ (show $ cs)
      putStrLn $ show acts1
      case acts1 of
           -- This is an error? Reset the chosen set.
           [] -> return (Nothing, State inputs [] ind db)
           _ -> case filter ((n `elem`) . snd) acts1 of
                  -- Clicked an irrelevant thing
                  [] -> return (Nothing, noop)
                  -- Apply choice
                  [choice] ->
                    let db' = insertTuple ind choice db
                    in return (Just $ encodeDB db', State inputs [] ind db')
                  _ -> return (Nothing, State inputs (nub $ n:choices) ind db)
    Just ev -> do
      let t = parseEvent ev
      let db' = insertTuple ind t db
      return (newdb db')
    _ -> return (Nothing, noop)
  where
    newdb db = (Just $ encodeDB db, State inputs choices ind db)

main = do
  (_,ind,inputs,db) <- mainUI
  runServer (State inputs [] ind db) handler
