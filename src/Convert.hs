{-# LANGUAGE OverloadedStrings #-}
module Convert
  ( readRules, readDBFile, runProgram
  , loadProgram, runProgramWithDB, programWithDB
  , runAnalysis
  )
  where

import Control.Monad (foldM)

import Types
import Parser
import Rules
import Monad
import Graph
import Update
import Reflection

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
    Right rs -> return $ convertRules rs
    Left err -> error $ err

-- 1. treats the input RHS as a set of tuples to add
-- 2. adds them, possibly extending context c
-- 3. solves for consequences, using rules
-- 4. returns extended context and a "root cause" tuple
processInputTuples :: [Rule] -> Context -> RHS -> M2 (Tuple, Context)
processInputTuples rules c es = do
  let initMatch t edges c =
        (Provenance (-1, Rule Event [] edges) (Just $ toEvent t) [] [], c, [])
  root <- makeTuple ("_root", []) externProv
  (msgs, c') <- applyMatch $ initMatch root es c
  solve rules msgs
  return (root, c')

-- returns a "root" tuple that can be used to access the results of each
-- block of edges.
programWithDB :: [RHS] -> [Rule] -> M2 [Tuple]
programWithDB edgeBlocks rules = prog2
    where
      prog1 (ts, c) es = do
        (t, c') <- processInputTuples rules c es
        return (t:ts, c')

      prog2 :: M2 [Tuple]
      prog2 = do
        -- set up queues/indices
        resetProcessor rules
        fst <$> foldM prog1 ([], []) edgeBlocks

runProgramWithDB e r = runDB (Nothing) emptyDB $ programWithDB e r

loadProgram :: FilePath -> FilePath -> IO ([RHS], [Rule])
loadProgram edgeFile ruleFile = do
    edgeBlocks <- readDBFile edgeFile
    rules <- readRules ruleFile

    return (edgeBlocks, rules)

-- Program execution
-- Main function
runProgram :: FilePath -> FilePath -> IO ([Tuple], [Rule], InterpreterState)
runProgram edgeName ruleName = do
    (edgeBlocks, rules) <- loadProgram edgeName ruleName
    let (roots, s) = runProgramWithDB edgeBlocks rules
    return (roots, rules, s)

runAnalysis :: [Rule] -> [Rule] -> M2 ()
runAnalysis rules metaRules = prog3
  where
    prog3 :: M2 ()
    prog3 = do
      flattenRules rules
      msgs <- flushEvents
      solve metaRules msgs
      return ()
