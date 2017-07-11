module Index where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort, sortOn)

import Data.Char

import Types
import Expr

dotClauses :: Rule -> LHS
dotClauses (LRule _ _) = []
dotClauses (Rule lhs _) = mapMaybe isdot lhs
  where
    isdot q@(Query High _) = Just q
    isdot _ = Nothing

linearClauses :: LHS -> LHS
linearClauses = mapMaybe islinear
  where
    islinear q@(Query _ (EP Linear _ _ _)) = Just q
    islinear _ = Nothing

-- TODO combine these?
insertRule :: Rule -> Index -> Index
insertRule rule@(Rule lhs _) ind =
  case dotClauses rule of
    [] -> foldr step ind lhs
    cs -> foldr step ind cs
  where
    pattern = S.fromList lhs
    step q@(Query _ ep) ind =
      M.insertWith (++) (epLabel ep, epSign ep)
                        [(linear, rule, q, S.delete q pattern)] ind
    step _ ind = ind
    -- TODO remove?
    linear = if not . null . linearClauses $ lhs then Linear else NonLinear

insertLRule :: Rule -> Index -> Index
insertLRule rule@(LRule lhs _) ind =
    foldr step ind lhs
  where
    pattern = S.fromList lhs
    step q@(Query _ ep) ind =
      M.insertWith (++) (epLabel ep, epSign ep) [(NonLinear, rule, q, S.delete q pattern)] ind
    step _ ind = ind

makeIndex :: [Rule] -> Index
makeIndex = foldr insertRule emptyIndex

indexLRule :: Rule -> Index
indexLRule rule = insertLRule rule emptyIndex

indexRule :: Rule -> Index
indexRule rule =
  case rule of
    r@(LRule _ _) -> indexLRule rule
    _ -> makeIndex [rule]


indLookup sig ind | Just v <- M.lookup sig ind = v
indLookup _ _ = []
