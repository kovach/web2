{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State (gets)
import System.Console.ANSI
import Data.List (sort, sortOn, intercalate)
--import System.Console.Readline

import Types
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
      cs = sortOn (negate . tid) $ filter ((== (Just $ Just root)) . fmap fe . tuple_src . source) ts
  in Node (root `elem` dead) root (map (makeTree dead ts) cs)

data IOMarker = Input | Output | Internal | Ignored
  deriving (Eq, Show, Ord)

setTupleColor is os as t = setSGR [SetColor Foreground Dull marker]
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

-- Main function
runProgram start_marker edgeName ruleName = do
    let prefix = "examples/"
        edgeFile = prefix ++ edgeName
        ruleFile = prefix ++ ruleName

    edges <- readDBFile edgeFile
    rules <- convert <$> readRules ruleFile

    let externalInputs = trueInputs start_marker rules edges

    let initMatch t = (Provenance (Rule [] edges) (Just $ toEvent t) [] [], [])

    let prog = do
          t <- rootTuple
          applyMatch $ initMatch t
          msgs <- flushEvents
          _ <- solve rules msgs
          l <- gets msgLog
          return (t, l)
        -- TODO remove rule params from runDB
        ((rootT, msgs), s2) = runDB (Nothing) emptyDB emptyIndex [] prog
        result = db s2
        resultTree = makeTree (removed_tuples result) (allTuples result) rootT

    return (msgs, rules, externalInputs, result, resultTree)

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
    (msgLog, rules, externalInputs, result, resultTree) <- runProgram start_marker edgeFile ruleFile

    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules
    putStrLn "external inputs:"
    mapM_ (putStrLn . ("  " ++) . show) $ externalInputs

    let tupleList = fromGraph $ tuples result

    -- TODO fix this
    --putStrLn "play actions:"
    --let index = undefined
    --mapM_ print $ concatMap (actions result index) externalInputs

    if do_print
      then do
        printTree rules resultTree
      else return ()
    putStrLn $ ppFacts (facts result)

    putStrLn "final tuple count:"
    print $ length tupleList

    if False then do
      putStrLn "msg log:"
      mapM_ (putStrLn . ppMsg) $ reverse msgLog
      else return ()

    -- random unit test
    let alltids = sort $ map tid (tupleList ++ removed_tuples result)
        -- rootTuple (tid 0) exists but isn't in db
        expectedids = [1..length alltids]
    if (expectedids /= alltids)
      then do
        warn "tids not consistent!!"
        print alltids
        putStrLn "missing:"
        print $ filter (not . (`elem` alltids)) expectedids
      else warn "tids consistent!"

    return ()

  where
    warn s = putStrLn $ "\n"++s++"\n"

p1 = runTextDemo "start_game" "card_game.graph" "card_game.arrow"
p2 = runTextDemo "start_turn" "game2.graph" "game2.arrow"
p3 = runTextDemo nullLabel "test.graph" "test.arrow"
p4 = runTextDemo "start_game" "go.graph" "go.arrow"

main = p2 True
mn = p4 False
