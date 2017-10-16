{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sortOn)
import Control.Monad (unless)
import Control.Monad.State
import System.Console.ANSI

import Types
import Monad
import Rules
import Convert

data PTree = TreeNode Bool Tuple [PTree]

makeTree :: [Tuple] -> [Tuple] -> Tuple -> PTree
makeTree dead ts root@(T{})=
  let cs = sortOn tval $ filter ((== (Just root)) . tuple_cause . source) ts
  in TreeNode (root `elem` dead) root (map (makeTree dead ts) cs)

data IOMarker = Input | Output | Internal | Ignored
  deriving (Eq, Show, Ord)

red = setColor Red
green = setColor Green
blue = setColor Blue
white = setColor White
setColor c = setSGR [SetColor Foreground Dull c]
setTupleColor is os as t = setColor marker
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
    p i (TreeNode dead t ts) = do
      putStr $ replicate (i-1) ' '
      --red
      putStr $ if dead then "_" else " "
      setTupleColor is os as t
      putStrLn $ ppTuple t
      setSGR [Reset]
      mapM_ (p (i+2)) ts

runTextDemo start_marker pr edgeFile ruleFile do_print = do
    let prefix s = pr++s
    (_, outputs, rules, s) <- runProgram (prefix edgeFile) (prefix ruleFile)
    let gasUsed = defaultGas - gas s
    let result = db s
    let logged = msgLog s

    --let resultTrees = map (makeTree (removed_tuples result) (allTuples result)) roots

    mapM print rules

    putStrLn "persistent relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ eventRelations rules
    putStrLn "dynamic relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ viewRelations rules
    putStrLn "reduced relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ reducedRelations rules
    putStrLn "input relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ inputRelations rules
    putStrLn "output relations:"
    mapM_ (putStrLn . ("  " ++) . show) $ outputRelations rules

    let tupleList = fromGraph $ tuples result

    if do_print
      then do
        --mapM_ (printTree rules) $ reverse resultTrees
        mapM_ (putStrLn . ppTupleProv) $ sortOn (abs . tid) tupleList
      else return ()

    -- SWITCH:
    if False then do
      putStrLn "msg log:"
      mapM_ (putStrLn) $ reverse logged
      else return ()

    putStrLn "final tuple count:"
    print $ length tupleList

    putStrLn "steps used:"
    print $ gasUsed

    unless (gasUsed < defaultGas) $ do
      red
      putStrLn "WARNING exhausted gas"
      white

    putStrLn "msgs sent:"
    print $ length outputs

    return ()

-- e.g. `runExample "turing"`
runExample :: FilePath -> IO ()
runExample s = runTextDemo nullLabel "examples/" (s++".graph") (s++".arrow") True

runP :: String -> FilePath -> IO ()
runP p s = runTextDemo nullLabel p (s++".graph") (s++".arrow") True

--p1 = runTextDemo "start_game" "card_game.graph" "card_game.arrow"
--p2 = runTextDemo "start_turn" "game2.graph" "game2.arrow"
--p3 = runTextDemo nullLabel "test.graph" "test.arrow"
--p4 = runTextDemo "start_game" "go.graph" "go.arrow"

main = do
  putStrLn "starting test"
  runTextDemo "start_game" "examples/" "go_stress.graph" "go.arrow" False
