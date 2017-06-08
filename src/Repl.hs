{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Repl where

import Control.Monad
import System.Console.ANSI
--import System.Console.Readline
import Data.List (sort, sortOn, intercalate)
import Data.Maybe (fromJust)

import Types
import Monad
import Rules
import Graph
import Reflection
import Parse
import Parser
import Index

rootTuple = makeTuple ("", []) Nothing

data PTree = Node Tuple [PTree] | RNode Tuple

makeTree :: [Tuple] -> Tuple -> PTree
makeTree ts root =
  let cs = sortOn tid $ filter ((== (Just root)) . fmap tuple_src . source) ts
  in Node root (map (makeTree ts) cs)

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
    p i (Node t ts) = do
      putStr $ replicate i ' '
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
    rules <- readRules ruleFile

    -- calculate user actions
    let externalInputs = trueInputs start_marker rules edges
    let index = makeIndex rules

    let prog = do
          t <- rootTuple
          applyMatch t (-1, emptyMatchBindings, edges)
          fixDB
          return t
        (rootT, s2) = runDB (Just $ 2^30) emptyDB index prog
        result = db s2
        resultTree = makeTree (allTuples result) rootT

    return (rules, index, externalInputs, result, resultTree)

  where

parseCommand str =
  case runParser int_ str of
    Right (i, "") -> Just $ NTRef i
    _ -> Nothing


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
    (rules, index, externalInputs, result, resultTree) <- runProgram start_marker edgeFile ruleFile

    mapM_ print $ rules
    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules
    putStrLn "external inputs:"
    mapM_ (putStrLn . ("  " ++) . show) $ externalInputs

    let tupleList = fromGraph $ tuples result

    putStrLn "play actions:"
    mapM_ print $ concatMap (actions result index) externalInputs

    if do_print then printTree rules resultTree else return ()

    putStrLn "final tuple count:"
    print $ length tupleList

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

main1 = runTextDemo "start_game" "card_game.graph" "card_game.arrow"
main2 = runTextDemo "start_turn" "game2.graph" "game2.arrow"
main3 = runTextDemo nullLabel "test.graph" "test.arrow"
mainUI = runProgram nullLabel "ui.txt" "ui.arrow"

main = main2 True
