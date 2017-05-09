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

encodeDB = encode . reverse . map tupleJSON . tuples

printer msg noop@(ind, db) = do
  case parseCommand msg of
    Just Reset -> do
      (_,ind',_,db') <- mainUI
      return (Just $ encodeDB db , (ind', db'))
    Just (Hover n) -> do
      putStrLn "hover"
      let db' = insertTuple ind ("hover", [n]) db
      return (Just $ encodeDB db', (ind, db'))
    Just (UnHover n) -> do
      putStrLn "unhover"
      let db' = insertTuple ind ("unhover", [n]) db
      return (Just $ encodeDB db', (ind, db'))
    Just (Click n) -> do
      putStrLn "click"
      let db' = insertTuple ind ("click", [n]) db
      return (Just $ encodeDB db', (ind, db'))
    _ -> return (Nothing, noop)

main = do
  (_,ind,_,db) <- mainUI
  runServer (ind, db) printer
