{-# LANGUAGE OverloadedStrings #-}
module Convert
  ( readRules, readDBFile, runProgram
  , loadProgram, runProgramWithDB, programWithDB)
  where

import Control.Monad
import Control.Monad.State

import Types
import Parser
import Rules
import Monad
import Graph
import Update
import Iterate

-- DB parsing
readDBFile :: FilePath -> IO [[Assert]]
readDBFile file = do
  f <- readFile file
  case parseTupleFile f of
    Right v -> return $ map labelRHSArity v
    Left err -> error $ "error parsing graph file:\n" ++ err

-- Program parsing
readRules :: FilePath -> IO [Rule]
readRules f = do
  rs <- readFile f
  case parseRuleFile rs of
    Right rs -> return $ convertRules $ map (\(l,a,_) -> (l,a)) rs

    Left err -> error $ err

-- 1. treats the input RHS as a set of tuples to add
-- 2. adds them, possibly extending context c
-- 3. solves for consequences, using rules
-- 4. returns extended context and a "root cause" tuple
processInputTuples :: PS -> Context -> RHS -> SM (Tuple, [Msg], Context, PS)
processInputTuples ps c es = do
  let initMatch t edges c =
        (Provenance (unsafeRanked (-1) $ Rule Nothing Event [] edges) (Just $ toEvent t) [] [], c, [])
  root <- lift $ makeTuple ("_root", []) externProv
  (msgs, c') <- lift $ applyMatch $ initMatch root es c
  (out, ps') <- solve (map CMsg msgs) ps
  return (root, out, c', ps')

-- returns a "root" tuple that can be used to access the results of each
-- block of edges.
programWithDB :: [RHS] -> [Rule] -> SM ([Tuple], [Msg], PS)
programWithDB edgeBlocks rules = prog2
    where
      prog1 (ts, outs, c, ps) es = do
        (t, out, c', ps') <- processInputTuples ps c es
        return (t:ts, out ++ outs, c', ps')

      prog2 :: SM ([Tuple], [Msg], PS)
      prog2 = do
        -- set up queues/indices
        db <- lift $ gets db
        ps <- lift $ initPS "some-program" rules (tuples db)
        fix <$> foldM prog1 ([], [], [], ps) edgeBlocks
      fix (a, ms, _, b) = (a, ms, b)

runProgramWithDB :: [RHS] -> [Rule] -> (([Tuple], [Msg], PS), InterpreterState)
runProgramWithDB e r = runStack1 $ programWithDB e r

loadProgram :: FilePath -> FilePath -> IO ([RHS], [Rule])
loadProgram edgeFile ruleFile = do
    edgeBlocks <- readDBFile edgeFile
    rules <- readRules ruleFile
    return (edgeBlocks, rules)

-- Program execution
-- Main function
runProgram :: FilePath -> FilePath -> IO ([Tuple], [Msg], [Rule], InterpreterState)
runProgram edgeName ruleName = do
    (edgeBlocks, rules) <- loadProgram edgeName ruleName
    let ((roots, ms, _), s) = runProgramWithDB edgeBlocks rules
    return (roots, ms, rules, s)
