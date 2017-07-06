{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State (gets)
import System.Console.ANSI
import Data.List (sort, sortOn, intercalate)
--import System.Console.Readline

import Types
import FactIndex
import Monad
import Convert
import Update
import Index
import Graph
import Rules
import Reflection


import Debug.Trace

rootTuple = makeTuple ("", []) (Provenance nullRule Nothing [] [])

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

metaFile = "examples/analysis.arrow"

-- Main function
runProgram start_marker edgeName ruleName = do
    let prefix = "examples/"
        edgeFile = prefix ++ edgeName
        ruleFile = prefix ++ ruleName

    edgeBlocks <- readDBFile edgeFile
    let edges = concat edgeBlocks
    rules <- convert <$> readRules ruleFile

    metaRules <- convert <$> readRules metaFile

    let externalInputs = trueInputs start_marker rules edges

    let initMatch t edges c = (Provenance (Rule [] edges) (Just $ toEvent t) [] [], c)

    let prog1 (ts, c) es = do
          t <- rootTuple
          c' <- applyMatch $ initMatch t es c
          msgs <- flushEvents
          _ <- solve rules msgs
          return (t:ts, c')
        prog2 :: M2 ([Tuple], [Msg])
        prog2 = do
          (roots, _) <- foldM prog1 ([], []) edgeBlocks
          l <- gets msgLog
          return (roots, l)
        prog3 :: M2 ()
        prog3 = do
          mapM_ flattenRule rules
          msgs <- flushEvents
          _ <- solve metaRules msgs
          return ()

        ((roots, msgs), s2) = runDB (Nothing) emptyDB prog2
        (_, s3) = runDB Nothing emptyDB prog3
        result = db s2
        ruleEmbedding = db s3
        resultTrees = map (makeTree (removed_tuples result) (allTuples result)) roots

    return (msgs, rules, externalInputs, result, resultTrees, defaultGas - gas s2, ruleEmbedding)

  where

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
    (msgLog, rules, externalInputs, result, resultTrees, gas, ruleEmbedding) <- runProgram start_marker edgeFile ruleFile

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
      mapM_ (putStrLn . ppMsg) $ reverse msgLog
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

    putStrLn "msgs sent:"
    print $ length msgLog

    return ()

runExample s = runTextDemo nullLabel (s++".graph") (s++".arrow") True

p1 = runTextDemo "start_game" "card_game.graph" "card_game.arrow"
p2 = runTextDemo "start_turn" "game2.graph" "game2.arrow"
p3 = runTextDemo nullLabel "test.graph" "test.arrow"
p4 = runTextDemo "start_game" "go.graph" "go.arrow"

main = p4 False
mn = p4 False
