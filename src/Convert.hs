{-# LANGUAGE OverloadedStrings #-}
module Convert
  ( readRules, readDBFile, runProgram
  , loadProgram, runProgramWithDB
  ) where

import Control.Monad.State (gets)
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
    Right rs -> return $ convert rs
    Left err -> error $ err

convert :: [LineRule] -> [Rule]
convert rs = result
  where
    -- TODO: keep line numbers for logical rules too; print on check failure
    heads = logicalRelations $ map snd rs

    result = map fix rs

    convertq (line, rule) q@(Query d ep@(EP Linear _ l ns)) | l `elem` heads = error $ "Rules may not consume logical tuples. error on line " ++ show line ++ ":\n" ++ show rule
    convertq _ (Query d ep@(EP _ _ l ns)) | l `elem` heads = Query d (LP Positive l ns)
    convertq _ q = q

    check (line, rule) (Assert l _) | l `elem` heads = error $ "Event rule (=>) may not assert logical tuple. error on line " ++ show line ++ ":\n" ++ show rule
    check _ a = a

    fix r@(_, Rule lhs rhs) = Rule (map (convertq r) lhs) (map (check r) rhs)
    -- TODO important: all variables in the rhs must be bound in the lhs
    fix r@(_, LRule lhs rhs) = LRule (map (convertq r) lhs) rhs

-- Program execution
-- Main function
-- TODO don't return huge tuple
runProgram start_marker edgeName ruleName = do
    (edgeBlocks, rules, metaRules) <- loadProgram edgeName ruleName

    let edges = concat edgeBlocks
    let externalInputs = trueInputs start_marker rules edges

    let (msgs, roots, result, outputs, gas) = runProgramWithDB edgeBlocks rules
    let ruleEmbedding = runAnalysis rules metaRules

    return (msgs, rules, externalInputs, result, outputs, roots, gas, ruleEmbedding)

loadProgram edgeFile ruleFile = do
    let metaFile = "examples/analysis.arrow"

    edgeBlocks <- readDBFile edgeFile
    rules <- readRules ruleFile

    metaRules <- readRules metaFile

    return (edgeBlocks, rules, metaRules)


runProgramWithDB edgeBlocks rules =
    let
      initMatch t edges c = (Provenance (Rule [] edges) (Just $ toEvent t) [] [], c)
      rootTuple = makeTuple ("", []) (Provenance nullRule Nothing [] [])
      prog1 (ts, c) es = do
          t <- rootTuple
          c' <- applyMatch $ initMatch t es c
          msgs <- flushEvents
          _ <- solve rules msgs
          return (t:ts, c')
      prog2 :: M2 ([Tuple], [String])
      prog2 = do
        (roots, _) <- foldM prog1 ([], []) edgeBlocks
        l <- gets msgLog
        return (roots, l)

      ((roots, msgs), s2) = runDB (Nothing) emptyDB prog2

      result = db s2
      outputs = netOutput s2
    in (msgs, roots, result, outputs, defaultGas - gas s2)

runAnalysis rules metaRules =
  let
    prog3 :: M2 ()
    prog3 = do
      mapM_ flattenRule rules
      msgs <- flushEvents
      _ <- solve metaRules msgs
      return ()

    (_, s3) = runDB Nothing emptyDB prog3

    ruleEmbedding = db s3
  in ruleEmbedding
