{-# LANGUAGE TupleSections #-}
module FactIndex
  (FactState, updateFact, updateEv, falsify, fsDiff, restrictFacts
  , fsMerge, emptyFS, toEvents, ppFS
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set, (\\))
import qualified Data.Set as S

import Types

-----------------------------------------------------
-- TODO surely there was an easier way to write this?
type FactMap = Map Fact [Provenance]
type FactMap2 = Map Label (Map [Node] [Provenance])
flatten2 :: FactMap2 -> FactMap
flatten2 = M.fromList . map assoc . concatMap flatten . map (second M.toList) . M.toList
  where
    flatten (a, bs) = zip (repeat a) bs
    assoc (a, (b, c)) = ((a, b), c)

lookMap :: Ord k  => k -> Map k (Map k2 v) -> Map k2 v
lookMap k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> M.empty

look2 :: Fact -> FactMap2 -> [Provenance]
look2 (l, ns) = lookList ns . lookMap l

keys2 :: FactMap2 -> Set Fact
keys2 = S.unions . map (\(k,ks) -> S.map (k,) ks) . M.toList . fmap (M.keysSet)

mfilter2 :: ([Provenance] -> Bool) -> FactMap2 -> FactMap2
mfilter2 = fmap . M.filter

adjust2 :: ([Provenance] -> [Provenance]) -> Fact -> FactMap2 -> FactMap2
adjust2 f (l, ns) = M.adjust (M.adjust f ns) l

merge2 :: FactMap2 -> FactMap2 -> FactMap2
merge2 = M.unionWith (M.unionWith (++))

clean2 :: FactMap2 -> FactMap2
clean2 = fmap clean

push2 (l, ns) p = M.insertWith (M.unionWith (++)) l (M.singleton ns [p])
-----------------------------------------------------

data Key = KT Polarity Tuple | KF Polarity Fact
  deriving (Eq, Show, Ord)

data FactState = FS
  { ind :: Map Key [(Fact, Provenance)]
  , fs :: FactMap2 }

emptyFS :: FactState
emptyFS = FS M.empty M.empty

toEvents :: Label -> FactState -> [Event]
toEvents l = map (\(ns, ps) -> EFact (l, ns) ps)
           . filter (not . null . snd)
           . M.toList . lookMap l . fs

ppFS :: FactState -> String
ppFS = unlines . map pp . M.toList . clean . flatten2 . fs
  where
    pp (f, ps) = unlines $ [show f ++ " : " ++ show (length ps)] ++ map (("  " ++) . ppMatch) ps

fsDiff :: FactState -> FactState -> [Event]
fsDiff (FS _ fs0) (FS _ fs') = map (fix Negative) gone ++ map (fix Positive) new
  where
    fs = clean2 fs0
    initial  = keys2 fs
    -- true things that became false
    gone = S.toList $ keys2 (mfilter2 null fs') `S.intersection` initial
    -- things that became true
    new = S.toList $ keys2 (clean2 fs') \\ initial
    -- turn Fact into Event
    fix Positive key = EFact key (look2 key fs')
    fix Negative key = EFalse key

fsMerge :: FactState -> FactState -> FactState
fsMerge (FS i1 f1) (FS i2 f2) = FS (M.unionWith (++) i1 i2) (merge2 f1 f2)

restrictFacts :: Rule -> FactState -> FactState
restrictFacts r@(LRule _ _) (FS ind fs) = FS ind' fs'
  where
    fix1 = clean . fmap (filter ((== r) . snd . rule_src . snd))
    fix2 = clean2 . fmap (fmap $ filter ((== r) . snd . rule_src))
    ind' = fix1 ind
    fs'  = fix2 fs
restrictFacts r _ = emptyFS

evKey :: Event -> Key
evKey ev =
  case ev of
    EFalse f -> KF Negative f
    EFact f _ -> KF Positive f
    E p t -> KT p t

updateFact :: Msg -> FactState -> FactState
updateFact (MF Positive f pr) s = updateAdd f pr s
updateFact (MF Negative f pr) s = updateRem f pr s
updateFact _ s = s

updateAdd f pr (FS ind fs) = (FS m1 m2)
  where
    pair = (f, pr)
    pairs = zip (map evKey $ matched pr) (repeat pair)
    m1 = foldr (\(e, p) m -> M.insertWith (++) e [p] m) ind pairs
    m2 = push2 f pr fs
updateRem f pr (FS ind fs) = (FS m1 m2)
  where
    pair = (f, pr)
    m1 = foldr (\e -> M.adjust (filter (/= pair)) e) ind (map evKey $ matched pr)
    m2 = adjust2 (filter (/= pr)) f fs

updateEv :: Event -> FactState -> FactState
updateEv (EFact f prs) s = foldr updateFact s $ map (MF Positive f) prs
-- TODO inline and avoid repeated filter?
updateEv (EFalse f) s@(FS _ fs) = foldr updateFact s $ map (MF Negative f) (look2 f fs)
updateEv _ s = s

-- Using an index significantly speeds up this step during logical rule update
falsify :: Event -> FactState -> ([Msg], FactState)
falsify ev s@(FS ind _) = (ms, s')
  where
    key = negKey (evKey ev)
    false = lookList key ind
    ms = map (uncurry $ MF Negative) false
    s' = foldr updateFact s ms

    negKey (KT p t) = KT (neg p) t
    negKey (KF p f) = KF (neg p) f
