module Rules where

import Data.Maybe (mapMaybe)
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as S

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

viewRelations :: [Rule] -> [Label]
viewRelations = nub . concatMap getLRHS
  where
    getLRHS Rule {rule_type = View, rhs = rhs} = map assertRel rhs
    getLRHS _ = []

reducedRelations :: [Rule] -> [(Label, ReduceOp)]
reducedRelations = nub . mapMaybe isValued . concatMap rhs
  where
    isValued (VAssert TValNull l _) = Just (l, ReduceOr)
    isValued (VAssert (TValExpr _) l _) = Just (l, ReduceSum)
    isValued _ = Nothing

eventRelations :: [Rule] -> [Label]
eventRelations rs = allRelations rs \\ viewRelations rs

labelAssertArity :: Assert -> Assert
labelAssertArity (Assert (L s) ns) = Assert (LA s (length ns)) ns
labelAssertArity (VAssert e (L s) ns) = VAssert e (LA s (length ns)) ns
labelAssertArity a = a
labelRHSArity = map labelAssertArity

labelQueryArity :: Query -> Query
labelQueryArity (Query d p) = Query d (fix p)
  where
    fix (EP lin (L l) ns) = EP lin (LA l (length ns)) ns
    fix (LP pol (L l) ns) = LP pol (LA l (length ns)) ns
    fix (VP v (L l) ns) = VP v (LA l (length ns)) ns
    fix p = p
labelQueryArity q = q
labelLHSArity = map labelQueryArity

-- "Type inference"
convertRules :: [(Int, Rule)] -> Either String [Rule]
convertRules rules = result
  where
    -- NOTE!
    --   relation/n is given the same type, "logical" or "event", for all n.
    --   bad convention?
    logRels = viewRelations $ map snd rules
    impRels = eventRelations $ map snd rules

    result = mapM fix rules

    convertq (line, rule) q@(Query d ep@(EP Linear l ns)) | l `elem` logRels =
      Left $ "Rules may not consume dynamical tuples. error on line " ++ show line ++ ":\n" ++ show rule
    convertq _ (Query d ep@(EP _ l ns)) | l `elem` logRels = Right $ Query d (LP Positive l ns)
    convertq (line, rule) (Query d lp@(LP _ l ns)) | l `elem` impRels =
      Left $ "Syntax error: `:` used with persistent relation: "++show l ++ ". error on line " ++ show line ++ ":\n" ++ show rule ++ "\n" ++ show impRels
    convertq _ q = Right q

    check (line, rule@(Rule {rule_type = Event})) (Assert l _) | l `elem` logRels =
      Left $ "Event rule (=>) may not assert dynamic tuple. error on line " ++ show line ++ ":\n" ++ show rule
    check (line, rule@(Rule {rule_type = View})) (Assert l _) | l `elem` impRels =
      Left $ "Logical rule (~>) may not assert persistent tuple. error on line " ++ show line ++ ":\n" ++ show rule
    check _ a = Right a

    fix r@(_, rule) = do
      lhs' <- mapM (convertq r) (lhs rule)
      rhs' <- mapM (check r) (rhs rule)
      let lhs'' = labelLHSArity lhs'
      let rhs'' = labelRHSArity rhs'
      return $ rule {lhs = lhs'', rhs = rhs''}
