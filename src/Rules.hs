module Rules where

import Types
import Expr
import Data.Maybe (mapMaybe)
import Data.List (nub)

lhsRels, rhsRels :: Rule -> [Label]
lhsRels rule = mapMaybe l (lhs rule)
  where
    l (Query _ ep) = Just (epLabel ep)
    -- l (Counter _ _) = error "unimplemented!"
    l _ = Nothing
rhsRels rule = map assertRel (rhs rule)

-- compute relations
--   appearing anywhere in rule set
--   only appearing on lhs of rules
--   only appearing on rhs of rules
allRelations, inputRelations, outputRelations :: [Rule] -> [Label]
allRelations = nub . concatMap rels
  where
    rels r = lhsRels r ++ rhsRels r

inputRelations rules = filter (\rel -> (not $ any (rel `elem`) $ rhsSets))
                              (allRelations rules)
  where
    rhsSets = map rhsRels rules

outputRelations rules = filter (\rel -> (not $ any (rel `elem`) $ lhsSets))
                              (allRelations rules)
  where
    lhsSets = map lhsRels rules

trueInputs :: Label -> [Rule] -> RHS -> [Label]
trueInputs l rules init = filter (not . (`elem` initRels)) $ inputRelations rules
  where
    initRels = takeWhile (/= l) (map assertRel init)

logicalRelations = nub . concatMap headRels . filter isLRule
  where
    headRels (LRule _ rhs) = map assertRel rhs
    isLRule (LRule _ _) = True
    isLRule _ = False
