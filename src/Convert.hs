module Convert where

import Data.List (nub, groupBy)
import Data.Maybe (mapMaybe)

import Types
import Parse
import Parser
import Rules

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

readRules :: FilePath -> IO [LineRule]
readRules f = do
  rs <- filterComment <$> readFile f
  return $ map (second parseLine) rs

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
