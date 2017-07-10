{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sort, sortOn, intercalate)
import Control.Monad (unless)
--import System.Console.Readline
import System.Console.ANSI

import Types
import FactIndex
import Monad
import Graph
import Rules
import Reflection
import Convert

import Debug.Trace

data PTree = Node Bool Tuple [PTree] | RNode Tuple

makeTree :: [Tuple] -> [Tuple] -> Tuple -> PTree
makeTree dead ts root@(T{})=
  let fe (E _ t) = Just t
      fe _ = Nothing
      cs = sortOn (tid) $ filter ((== (Just $ Just root)) . fmap fe . tuple_src . source) ts
      --cs = sortOn (negate . tid) $ filter ((== (Just $ Just root)) . fmap fe . tuple_src . source) ts
  in Node (root `elem` dead) root (map (makeTree dead ts) cs)

data IOMarker = Input | Output | Internal | Ignored
  deriving (Eq, Show, Ord)

red = setColor Red
green = setColor Green
blue = setColor Blue
white = setColor White
setColor c = setSGR [SetColor Foreground Dull c]
setTupleColor is os as t = setColor marker
  where
    marker =
      let l = label t in
      if l `elem` is then White
      else if l `elem` os then Blue
      else if l `elem` as then Green
      else Yellow

printTree rules = p 0
  where
    is = inputRelations rules
    os = outputRelations rules
    as = allRelations rules
    p i (Node dead t ts) = do
      putStr $ replicate (i-1) ' '
      setSGR [SetColor Foreground Dull Red]
      putStr $ if dead then "_" else " "
      setTupleColor is os as t
      putStrLn $ ppTuple t
      setSGR [Reset]
      mapM_ (p (i+2)) ts

--TODO fix
--parseCommand str =
--  case runParser int_ str of
--    Right (i, "") -> Just $ NTRef i
--    _ -> Nothing
--repl i@(rules, _, inputs, db, _) = do
--  ml <- readline "% "
--  case ml of
--    Nothing -> return ()
--    Just line -> do
--      addHistory line
--      case parseCommand line of
--        Just obj -> mapM_ (putStrLn . ppTuple) (attributes obj db)
--        _ -> return ()
--      repl i
--
--runRepl start_marker edgeFile ruleFile = do
--  output <- runProgram start_marker edgeFile ruleFile
--  repl output
-- repl1 = runRepl "start_game" "graph.txt" "rules.arrow" >> return ()

runTextDemo start_marker edgeFile ruleFile do_print = do
    let prefix s = "examples/" ++ s
    (msgLog, rules, externalInputs, result, outputs, roots, gas, ruleEmbedding) <- runProgram start_marker (prefix edgeFile) (prefix ruleFile)

    let resultTrees = map (makeTree (removed_tuples result) (allTuples result)) roots

    putStrLn "imperative relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ allRelations rules `diffList` logicalRelations rules
    putStrLn "logical relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ logicalRelations rules
    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "external inputs:"
    mapM_ (putStrLn . ("  " ++) . show) $ externalInputs
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules

    let tupleList = fromGraph $ tuples result

    -- TODO fix this
    --putStrLn "play actions:"
    --let index = undefined
    --mapM_ print $ concatMap (actions result index) externalInputs

    if do_print
      then do
        mapM_ (printTree rules) $ reverse resultTrees
      else return ()

    -- SWITCH:
    if False then do
      putStrLn "msg log:"
      mapM_ (putStrLn) $ reverse msgLog
      else return ()

    green
    putStrLn "\nfact state:"
    white
    putStrLn $ ppFS (facts result)

    -- SWITCH:
    if False then do
      red
      putStrLn "rule embedding:"
      white
      mapM_ (putStrLn . ppTuple) . (fromGraph . tuples) $ ruleEmbedding
      putStrLn $ ppFS (facts ruleEmbedding)
      putStrLn ""
      else return ()

    putStrLn "final tuple count:"
    print $ length tupleList

    putStrLn "steps used:"
    print $ gas

    unless (gas < defaultGas) $ do
      red
      putStrLn "WARNING exhausted gas"
      white

    putStrLn "msgs sent:"
    print $ length outputs

    return ()

runExample s = runTextDemo nullLabel (s++".graph") (s++".arrow") True

p1 = runTextDemo "start_game" "card_game.graph" "card_game.arrow"
p2 = runTextDemo "start_turn" "game2.graph" "game2.arrow"
p3 = runTextDemo nullLabel "test.graph" "test.arrow"
p4 = runTextDemo "start_game" "go.graph" "go.arrow"

main = p4 False
