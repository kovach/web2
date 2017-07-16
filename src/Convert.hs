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
    Right v -> return v
    Left err -> error $ "error parsing graph file:\n" ++ err

-- Program parsing
readRules :: FilePath -> IO [Rule]
readRules f = do
  rs <- readFile f
  case parseRuleFile rs of
    Right rs -> return $ convertRules rs
    Left err -> error $ err

convertRules :: [LineRule] -> [Rule]
convertRules rs = result
  where
    -- TODO: keep line numbers for logical rules too; print on check failure
    logRels = logicalRelations $ map snd rs
    impRels = allRelations (map snd rs) `diffList` logRels

    result = map fix rs

    convertq (line, rule) q@(Query d ep@(EP Linear _ l ns)) | l `elem` logRels =
      error $ "Rules may not consume logical tuples. error on line " ++ show line ++ ":\n" ++ show rule
    convertq _ (Query d ep@(EP _ _ l ns)) | l `elem` logRels = Query d (LP Positive l ns)
    convertq (line, _) (Query d lp@(LP _ l ns)) | l `elem` impRels =
      error $ "Cannot negate event relation: "++show l ++ ". error on line " ++ show line ++ "."
    convertq _ q = q

    check (line, rule) (Assert l _) | l `elem` logRels =
      error $ "Event rule (=>) may not assert logical tuple. error on line " ++ show line ++ ":\n" ++ show rule
    check _ a = a

    fix r@(_, Rule lhs rhs) = Rule (map (convertq r) lhs) (map (check r) rhs)
    -- TODO important: all variables in the rhs must be bound in the lhs
    fix r@(_, LRule lhs rhs) = LRule (map (convertq r) lhs) rhs

-- 1. treats the input RHS as a set of tuples to add
-- 2. adds them, possibly extending context c
-- 3. solves for consequences, using rules
-- 4. returns extended context and a "root cause" tuple
processInputTuples :: [Rule] -> Context -> RHS -> M2 (Tuple, Context)
processInputTuples rules c es = do
  let initMatch t edges c = (Provenance (Rule [] edges) (Just $ toEvent t) [] [], c)
  root <- makeTuple ("", []) externProv
  (msgs, c') <- applyMatch $ initMatch root es c
  --msgs <- flushEvents
  _ <- solve rules msgs
  return (root, c')

-- returns a "root" tuple that can be used to access the results of each
-- block of edges.
programWithDB :: [RHS] -> [Rule] -> M2 [Tuple]
programWithDB edgeBlocks rules = prog2
    where
      prog1 (ts, c) es = do
        (t, c') <- processInputTuples rules c es
        return (t:ts, c)

      prog2 :: M2 [Tuple]
      prog2 = fst <$> foldM prog1 ([], []) edgeBlocks

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
      mapM_ flattenRule rules
      msgs <- flushEvents
      _ <- solve metaRules msgs
      return ()

    --(_, s3) = runDB Nothing emptyDB prog3
