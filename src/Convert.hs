{-# LANGUAGE OverloadedStrings #-}
module Convert
  ( readRules, readDBFile, runProgram
  , loadProgram, runProgramWithDB
  ) where

import Data.List (nub, groupBy)
import Data.Maybe (mapMaybe)
import Control.Monad.State (gets)
import Control.Monad (foldM)

import Types
import Parse
import Parser
import Rules
-- for runProgram:
import Monad
import Graph
import Update
import Reflection

filterComment :: String -> [(Int, String)]
filterComment = filter (not . null . snd) . zip [1..] . map (takeWhile (/= '#')) . lines

filterComments = lines .> zip [1..] .> blocks .>
                 map (map (second $ takeWhile (/= '#')) .> nonEmpty)
  where
    blocks [] = []
    blocks xs = let (x, r) = span (not . null . snd) xs in x : blocks (st r)
    st [] = []
    st (_:a) = a
    nonEmpty = filter (not . null . snd)
    (.>) = flip (.)

-- DB parsing
parseTuple :: String -> Maybe Assert
parseTuple s =
  case words s of
    _ -> case runParser rquery_ s of
           Left e -> error $ "error parsing graph file: " ++ show e
           Right (a, "") -> Just a
           _ -> error "error parsing graph file."

readDBFile :: FilePath -> IO [[Assert]]
readDBFile file = do
  f <- readFile file
  return $ map (mapMaybe parseTuple . map snd) $ filterComments f

-- Program parsing
type LineRule = (Int, Rule)

readRules :: FilePath -> IO [Rule]
readRules f = do
  rs <- filterComment <$> readFile f
  return $ convert $ map (second parseLine) rs

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

loadProgram edgeName ruleName = do
    let prefix = "examples/"
        edgeFile = prefix ++ edgeName
        ruleFile = prefix ++ ruleName
        metaFile = "examples/analysis.arrow"

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
