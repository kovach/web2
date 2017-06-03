module Rules where

import Types
import Expr
import Data.Maybe (mapMaybe)
import Data.List (nub)

lhsRels, rhsRels :: Rule -> [Label]
lhsRels (Rule lhs _) = mapMaybe l lhs
  where
    l (Query _ (EP _ _ rel _)) = Just rel
    l (Counter _ _) = error "unimplemented!"
    l _ = Nothing
rhsRels (Rule _ rhs) = map assertRel rhs

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


data IOMarker = Input | Output | Internal | Ignored
  deriving (Eq, Show, Ord)
tupleIOType :: [Rule] -> Tuple -> IOMarker
tupleIOType rules t =
  let l = label t in
  if l `elem` inputRelations rules then Input
  else if l `elem` outputRelations rules then Output
  else if l `elem` allRelations rules then Internal
  else Ignored

trueInputs :: Label -> [Rule] -> RHS -> [Label]
trueInputs l rules init = filter (not . (`elem` initRels)) $ inputRelations rules
  where
    initRels = takeWhile (/= l) (map assertRel init)
