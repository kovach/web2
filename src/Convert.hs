-- Basic functions for running programs stored as text files
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
  f <- readFile f
  let result = convertRules =<< parseRuleFile f
  case result of
    Right rules -> return $ rules
    Left err -> error $ err

-- 1. treats the input RHS as a set of tuples to add
-- 2. adds them, possibly extending context c
-- 3. solves for consequences, using rules
-- 4. returns extended context and a "root cause" tuple
processInputTuples :: PS -> Context -> RHS -> SM (Tuple, [Msg], Context, PS)
processInputTuples ps c es = do
  let initMatch t edges c =
        (Provenance (unsafeRanked (-1) $ Rule Nothing Nothing Event [] edges) (Just $ toEvent t) [] [], c, [])
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

-- edgeFile: a file containing tuples, one per line.
--    empty lines separate blocks. each block is added simultaneously;
--    a fixed point is computed after each block
-- ruleFile: a file containing an ordered list of rules, one per line.
--    rules may not be split across lines.
loadProgram :: FilePath -> FilePath -> IO ([RHS], [Rule])
loadProgram edgeFile ruleFile = do
    edgeBlocks <- readDBFile edgeFile
    rules <- readRules ruleFile
    return (edgeBlocks, rules)

-- Main program execution function
--    returns result, but not the full execution context
runProgram :: FilePath -> FilePath -> IO ([Tuple], [Msg], [Rule], InterpreterState)
runProgram edgeName ruleName = do
    (edgeBlocks, rules) <- loadProgram edgeName ruleName
    let ((roots, ms, _), s) = runProgramWithDB edgeBlocks rules
    return (roots, ms, rules, s)
