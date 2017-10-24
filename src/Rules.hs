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
reducedRelations = nub . mapMaybe isV . concatMap rhs
  where
    isV (VAssert TValNull l _) = Just (l, ReduceOr)
    isV (VAssert (TValExpr _) l _) = Just (l, ReduceSum)
    isV _ = Nothing

eventRelations :: [Rule] -> [Label]
eventRelations = nub . mapMaybe ok . concatMap rhs . filter ((== Event) . rule_type)
  where
    ok (Assert l _) = Just l
    ok _ = Nothing

--eventRelations :: [Rule] -> [Label]
--eventRelations rs = allRelations rs \\ viewRelations rs

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

-- "Type checking"
--
-- TODO better treatment of input relations
--   should check they are used consistently
convertRules :: [(Int, Rule)] -> Either String [Rule]
convertRules rules = result
  where
    -- NOTE!
    --   relation/n is given the same type, "logical" or "event", for all n.
    --   bad convention?
    --    TODO: yes. fix
    viewRels = viewRelations $ map snd rules
    reducedRels = map fst $ reducedRelations $ map snd rules
    impRels = eventRelations $ map snd rules

    result = mapM fix rules

    notReduced l = l `elem` impRels || (l `elem` viewRels && not (l `elem` reducedRels))

    -- note, ..:p is not accepted by parser
    convertq (line, rule) q@(Query _ (EP Linear l _)) | l `elem` viewRels =
      Left $ "Rules may not consume tuples of dynamic relations. error on line " ++ show line ++ ":\n" ++ show rule
    convertq (line, rule) (Query _ (LP _ l _)) | notReduced l =
      Left $ "Syntax error: `:` used with non-reduced relation: "++show l ++ ". error on line " ++ show line ++ ":\n" ++ show rule ++ "\n" ++ show impRels
    convertq (line, rule) (Query _ (VP _ l _)) | notReduced l =
      Left $ "Syntax error: `:` used with non-reduced relation: "++show l ++ ". error on line " ++ show line ++ ":\n" ++ show rule
    convertq _ q = Right q

    check (line, rule@(Rule {rule_type = Event})) (Assert l _) | l `elem` viewRels =
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
