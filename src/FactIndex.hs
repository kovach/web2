module FactIndex
  (FactState, updateFact, updateEv, falsify, fsDiff, restrictFacts
  , fsMerge, emptyFS, toEvents, ppFS
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set, (\\))
import qualified Data.Set as S (toList, intersection)

import Types

data Key = KT Polarity Tuple | KF Polarity Fact
  deriving (Eq, Show, Ord)

data FactState = FS
  { ind :: Map Key [(Fact, Provenance)]
  , fs :: Map Fact [Provenance] }

emptyFS :: FactState
emptyFS = FS M.empty M.empty

-- TODO optimize this
toEvents :: Label -> FactState -> [Event]
toEvents l = map (uncurry EFact)
           . filter (not . null . snd) . filter (f . fst)
           . M.toList . fs
  where
    f (l', _) = l' == l

ppFS :: FactState -> String
ppFS = unlines . map pp . M.toList . clean . fs
  where
    pp (f, ps) = unlines $ [show f ++ " : " ++ show (length ps)] ++ map (("  " ++) . ppMatch) ps

fsDiff :: FactState -> FactState -> [Event]
fsDiff (FS _ fs0) (FS _ fs') = map (fix Negative) gone ++ map (fix Positive) new
  where
    fs = clean fs0
    initial  = M.keysSet fs
    -- true things that became false
    gone = S.toList $ M.keysSet (M.filter null fs') `S.intersection` initial
    -- things that became true
    new = S.toList $ M.keysSet (clean fs') \\ initial
    -- turn Fact into Event
    fix Positive key = EFact key (look key fs')
    fix Negative key = EFalse key

fsMerge :: FactState -> FactState -> FactState
fsMerge (FS i1 f1) (FS i2 f2) = FS (M.unionWith (++) i1 i2) (M.unionWith (++) f1 f2)

restrictFacts :: Rule -> FactState -> FactState
restrictFacts r@(LRule _ _) (FS ind fs) = FS ind' fs'
  where
    fix op = clean . M.map (filter ((== r) . rule_src . op))
    ind' = fix snd ind
    fs' = fix id fs
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

updateAdd f pr (FS ind fs) = (FS m1 m2)
  where
    pair = (f, pr)
    pairs = zip (map evKey $ matched pr) (repeat pair)
    m1 = foldr (\(e, p) m -> M.insertWith (++) e [p] m) ind pairs
    m2 = M.insertWith (++) f [pr] fs
updateRem f pr (FS ind fs) = (FS m1 m2)
  where
    pair = (f, pr)
    m1 = foldr (\e -> M.adjust (filter (/= pair)) e) ind (map evKey $ matched pr)
    m2 = M.adjust (filter (/= pr)) f fs

updateEv :: Event -> FactState -> FactState
updateEv (EFact f prs) s = foldr updateFact s $ map (MF Positive f) prs
-- TODO inline and avoid repeated filter?
updateEv (EFalse f) s@(FS _ fs) = foldr updateFact s $ map (MF Negative f) (lookList f fs)
updateEv _ s = s

-- Using an index significantly speeds up this step during logical rule update
falsify :: Event -> FactState -> ([Msg], FactState)
falsify ev s@(FS ind fs) = (ms, s')
  where
    key = negKey (evKey ev)
    false = lookList key ind
    ms = map (uncurry $ MF Negative) false
    s' = foldr updateFact s ms

    negKey (KT p t) = KT (neg p) t
    negKey (KF p f) = KF (neg p) f
