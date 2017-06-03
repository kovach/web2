{-# LANGUAGE TupleSections #-}
module Repl where

import Control.Monad
import System.Console.ANSI
import System.Console.Readline
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

setTupleColor rules t = setSGR [SetColor Foreground Dull c]
  where
    c = (fromJust $ lookup (tupleIOType rules t) colorMapping)
    colorMapping =
      [ (Input, White)
      , (Output, Blue)
      , (Internal, Green)
      , (Ignored, Yellow) ]

printTree rules = p 0
  where
    p i (Node t ts) = do
      putStr $ replicate i ' '
      setTupleColor rules t
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
          applyMatch t (0, emptyMatchBindings, edges)
          fixDB
          return t
        (rootT, s2) = runDB emptyDB index prog
        result = db s2
        resultTree = makeTree (allTuples result) rootT

    return (rules, index, externalInputs, result, resultTree)

  where

parseCommand str =
  case runParser int_ str of
    Right (i, "") -> Just $ NTRef i
    _ -> Nothing


repl i@(rules, _, inputs, db, _) = do
  ml <- readline "% "
  case ml of
    Nothing -> return ()
    Just line -> do
      addHistory line
      case parseCommand line of
        Just obj -> mapM_ (putStrLn . ppTuple) (attributes obj db)
        _ -> return ()
      repl i

runRepl start_marker edgeFile ruleFile = do
  output <- runProgram start_marker edgeFile ruleFile
  repl output

runTextDemo start_marker edgeFile ruleFile = do
    (rules, index, externalInputs, result, resultTree) <- runProgram start_marker edgeFile ruleFile

    mapM_ print $ rules
    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules
    putStrLn "external inputs:"
    mapM_ (putStrLn . ("  " ++) . show) $ externalInputs

    let tupleList = fromGraph $ tuples result

    --let log = sortOn (tid . snd) $ (map (True,) $ tupleList) ++ (map (False,) $ removed_tuples result)
    --putStrLn "\ngame log:"
    --mapM_ (colorTuple rules) $ log

    putStrLn "play actions:"
    mapM_ print $ concatMap (actions result index) externalInputs

    printTree rules resultTree

    putStrLn "final tuple count:"
    print $ length (tuples result)

    -- random unit test
    let alltids = sort $ map tid (tupleList ++ removed_tuples result)
        -- rootTuple exists but isn't in db
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
    colorTuple rules (alive, t) = do
      let debug = True
      let ddebug = True
      let str = ppTuple t
      setTupleColor rules t
      putStrLn str
      setSGR [Reset]

repl1 = runRepl "start_game" "graph.txt" "rules.arrow" >> return ()

main1 = runTextDemo "start_game" "card_game.graph" "card_game.arrow"
main2 = runTextDemo "start_turn" "scheme.graph" "scheme.arrow"
main3 = runTextDemo "" "test.graph" "test.arrow"
mainUI = runProgram "" "ui.txt" "ui.arrow"

main = main2
