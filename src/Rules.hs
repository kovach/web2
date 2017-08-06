module Rules where

import Data.Maybe (mapMaybe)
import Data.List (nub, (\\))

import Types
import Parser (LineRule)

lhsRels, rhsRels :: Rule -> [Label]
lhsRels = nub . mapMaybe l . lhs
  where
    l (Query _ ep) = Just (epLabel ep)
    l _ = Nothing
rhsRels = nub . map assertRel . rhs

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

logicalRelations :: [Rule] -> [Label]
logicalRelations = nub . concatMap getLRHS
  where
    getLRHS (Rule View _ rhs) = map assertRel rhs
    getLRHS _ = []

eventRelations :: [Rule] -> [Label]
eventRelations rs = allRelations rs \\ logicalRelations rs

labelAssertArity :: Assert -> Assert
labelAssertArity (Assert (L s) ns) = Assert (LA s (length ns)) ns
labelAssertArity a = a
labelRHSArity = map labelAssertArity

labelQueryArity :: Query -> Query
labelQueryArity (Query d p) = Query d (fix p)
  where
    fix (EP lin (L l) ns) = EP lin (LA l (length ns)) ns
    fix (LP pol (L l) ns) = LP pol (LA l (length ns)) ns
    fix p = p
labelQueryArity q = q
labelLHSArity = map labelQueryArity

-- "Type inference"
convertRules :: [LineRule] -> [Rule]
convertRules rs = result
  where
    -- NOTE!
    --   relation/n is given the same type, "logical" or "event", for all n.
    --   bad convention?
    rules = map (\(l,a,_) -> (l,a)) rs
    logRels = logicalRelations $ map snd rules
    impRels = eventRelations $ map snd rules

    result = map fix rules

    convertq (line, rule) q@(Query d ep@(EP Linear l ns)) | l `elem` logRels =
      error $ "Rules may not consume logical tuples. error on line " ++ show line ++ ":\n" ++ show rule
    convertq _ (Query d ep@(EP _ l ns)) | l `elem` logRels = Query d (LP Positive l ns)
    convertq (line, rule) (Query d lp@(LP _ l ns)) | l `elem` impRels =
      error $ "Cannot negate event relation: "++show l ++ ". error on line " ++ show line ++ ":\n" ++ show rule
    convertq _ q = q

    check (line, rule@(Rule Event _ _)) (Assert l _) | l `elem` logRels =
      error $ "Event rule (=>) may not assert logical tuple. error on line " ++ show line ++ ":\n" ++ show rule
    check (line, rule@(Rule View _ _)) (Assert l _) | l `elem` impRels =
      error $ "Logical rule (~>) may not assert event tuple. error on line " ++ show line ++ ":\n" ++ show rule
    check _ a = a

    fix r@(_, rule) = result
      where
        result = Rule (rtype rule) lhs'' rhs''
        lhs' = map (convertq r) (lhs rule)
        rhs' = map (check r) (rhs rule)

        lhs'' = labelLHSArity lhs'
        rhs'' = labelRHSArity rhs'
