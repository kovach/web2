{-# LANGUAGE TupleSections #-}
module Repl where

import Control.Monad
import System.Console.ANSI
import System.Console.Readline
import Data.List (sort, sortOn)

import Types
import Rules
import Increment
import Reflection
import Parse

parseCommand str =
  case runParser int_ str of
    Right (i, "") -> Just $ NTRef i
    _ -> Nothing


repl i@(rules, _, inputs, db) = do
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
    (rules, index, externalInputs, result) <- runProgram start_marker edgeFile ruleFile

    mapM_ print $ rules
    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules
    putStrLn "external inputs:"
    mapM_ (putStrLn . ("  " ++) . show) $ externalInputs

    let log = sortOn (revT . ts . snd) $ (map (True,) $ tuples result) ++ (map (False,) $ removed_tuples result)

    putStrLn "\ngame log:"
    mapM_ (colorTuple rules) $ log

    putStrLn "play actions:"
    mapM_ print $ actions result index "play"
    putStrLn "actions:"
    mapM_ print $ actions result index "end_turn"
    putStrLn "actions:"
    mapM_ print $ actions result index "do_attack"

    return ()

  where
    colorTuple rules (alive, t) = do
      let debug = True
      let ddebug = True
      let str = (if alive then "" else "    ") ++ ppTuple t
      case tupleIOType rules t of
        Input -> do
          setSGR [SetColor Foreground Vivid White]
          putStrLn str
        Output -> do
          setSGR [SetColor Foreground Dull Blue]
          putStrLn str
        Internal -> unless (not debug) $ do
          setSGR [SetColor Foreground Dull Green]
          putStrLn str
        Ignored -> unless (not ddebug) $ do
          setSGR [SetColor Foreground Dull Yellow]
          putStrLn str
      setSGR [Reset]

repl1 = runRepl "start_game" "graph.txt" "rules.arrow" >> return ()

main1 = runTextDemo "start_game" "graph.txt" "rules.arrow"
main2 = runTextDemo "" "graph2.txt" "parser.arrow"
main3 = runTextDemo "" "graph3.txt" "linearity.arrow"

