module Index where

import Data.List (nub)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

import Types

dotClauses :: Rule -> LHS
dotClauses = mapMaybe isdot . lhs
  where
    isdot q@(Query High _) = Just q
    isdot _ = Nothing

isLinear :: Rule -> Bool
isLinear (Rule {rule_type = Event, lhs = lhs}) = not . null . linearClauses $ lhs
isLinear _ = False

linearClauses :: LHS -> LHS
linearClauses = mapMaybe islinear
  where
    islinear q@(Query _ (EP Linear _ _)) = Just q
    islinear _ = Nothing

insertRule :: Rule -> Index -> Index
insertRule rule@(Rule {rule_type = Event, lhs = lhs0} ) ind =
  case dotClauses rule of
    [] -> foldr step ind lhs
    cs -> foldr step ind cs
  where
    -- TODO forbid repeated clauses?
    lhs = nub lhs0
    pattern = S.fromList lhs
    step q@(Query _ ep) ind =
      M.insertWith (++) (epLabel ep, epSign ep)
                        [(linear, rule, q, S.delete q pattern)] ind
    step _ ind = ind
    -- TODO remove?
    linear = if not . null . linearClauses $ lhs then Linear else NonLinear

insertRule rule@(Rule {rule_type = View, lhs = lhs} ) ind =
    foldr step ind (nub lhs)
  where
    pattern = S.fromList lhs
    step q@(Query _ ep) ind =
      M.insertWith (++) (epLabel ep, epSign ep)
        [(NonLinear, rule, q, S.delete q pattern)] ind
    step _ ind = ind

indexRule :: Rule -> Index
indexRule rule = insertRule rule emptyIndex

indLookup sig ind | Just v <- M.lookup sig ind = v
indLookup _ _ = []
