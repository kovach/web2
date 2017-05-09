module Reflection where

import Control.Monad
import Data.Maybe

import Types
import Expr
import Graph
import Rules
import Parser2
import Parse
import Increment

-- TODO last.unfold is dumb
insertTuple :: Index -> (Label, [Node]) -> DB -> DB
insertTuple index edge db = scheduleDB . last $ unfold (stepS index) schedule
  where
    (db', tuple) = makeTuple db edge
    schedule = (0, [tuple], db')


uiRels = ["box", "child", "color", "clear", "hide"]

pullRells :: [Label] -> DB -> [Tuple]
pullRells ls db = filter ok (tuples db)
  where
    ok t = label t `elem` ls
