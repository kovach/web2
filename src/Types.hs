-- Language notes in docs/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.String
import Data.List (intercalate, delete)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

data Label = L String | LA String Int | LRaw String Int
  deriving (Eq, Ord)

fromRaw :: Label -> Label
fromRaw (LRaw s i) = LA s i
fromRaw l = error "fromRaw: expect only tuples tagged with arity"

toRaw :: Label -> Label
toRaw (LA s i) = LRaw s i
toRaw l = error "toRaw: expect only tuples tagged with arity"

notRaw m | LRaw{} <- mlabel m = False
notRaw _ = True

instance IsString Label where
  fromString = L
instance Show Label where
  show (L s) = s
  -- TODO don't show arity
  show (LA s a) = s++"/"++show a
  show (LRaw l i) = l ++ "/RAW-"++show i

lstring :: Label -> String
lstring (L s) = s
lstring (LA s _) = s

nullLabel = L ""

-- TODO rename constructors
data Node = NInt Int | NNode Int | NSymbol String | NString String
  deriving (Eq, Ord)

data Polarity = Positive | Negative
  deriving (Eq, Show, Ord)

neg :: Polarity -> Polarity
neg Positive = Negative
neg Negative = Positive

toEvent t = t

-- TODO remove
type Event = Tuple

type Dependency = [Event]
type Consumed = [Tuple]

type RuleId = Int
type RankedRule = (RuleId, Rule)

-- TODO
-- sum, argmax (or "most recent")
data RedOp = Or
  deriving (Eq, Show, Ord)

-- An instance of a match
-- TODO include context bindings?
data Provenance = Provenance
  -- The rule of this match
  { rule_src :: RankedRule
  -- The tuple that triggered this match instance
  -- Nothing for rules with empty LHS, or external inputs
  , tuple_src :: Maybe Event
  -- Tuples matched by this match instance
  , matched :: Dependency
  -- Tuples removed from the world by this match instance
  , consumed :: Consumed
  }
  | Extern [Int] -- TODO ??
  | Reduction { reduction_op :: RedOp, reduced :: [Tuple] }
  deriving (Eq, Show, Ord)

type CProof = (Provenance, [Tuple])

tuple_cause p@(Provenance{}) = tuple_src p
tuple_cause Reduction{} = Nothing
tuple_cause Extern{} = Nothing

nullProv :: Provenance
nullProv = Provenance (1,nullRule) Nothing [] []

externProv = Extern []

type RawTuple = (Label, [Node])
type Fact = RawTuple

data Id = Id Int | Truth Bool
  deriving (Eq, Ord)

instance Show Id where
  show (Id i) = "#"++show i
  show (Truth b) = "#"++show b

-- TODO add id back; also to Provenance
data Tuple
  = T
  { nodes :: [Node]
  , label :: Label
  , source :: Provenance
  , tval :: Id }
  deriving (Show)

tfact :: Tuple -> Fact
tfact t = (label t, nodes t)

reducedTuple :: RedOp -> Fact -> [Provenance] -> Tuple
reducedTuple op f@(l,ns) ps = T ns l (Reduction op (map (trueTuple f) ps)) (Truth True)

trueTuple :: Fact -> Provenance -> Tuple
trueTuple (l, ns) p = T ns l p (Truth True)

falseTuple :: Fact -> Tuple
falseTuple (l, ns) = T ns l (Reduction Or []) (Truth False)

instance Eq Tuple where
  t1 == t2 =
    case (tval t1, tval t2) of
      (Id i1, Id i2) -> i1 == i2
      (v1, v2) -> v1 == v2 && tfact t1 == tfact t2

instance Ord Tuple where
  --t1 `compare` t2 = tid t1 `compare` tid t2
  t1 `compare` t2 =
    case (tval t1, tval t2) of
      (Id i1, Id i2) -> cmp1
        where
          cmp1 = i1 `compare` i2
      (v1, v2) -> cmp2
        where
          cmp2 = (tfact t1, v1) `compare` (tfact t2, v2)

instance IsString Node where
  fromString = NSymbol

instance Show Node where
  show (NInt i) = show i
  show (NNode i) = "#"++show i
  show (NSymbol s) = "'"++s
  show (NString s) = show s

type Count = Int

--type Graph = Map Label [Tuple]
--
--insertTuple :: Tuple -> Graph -> Graph
--insertTuple t = M.insertWith (++) (label t) [t]
--
--removeTuple :: Tuple -> Graph -> Graph
--removeTuple t@(T {tval = Id _}) = rt1 t
--removeTuple t = rt2 t
--rt1 t = M.adjust (delete t) (label t)
--rt2 t = M.adjust (delete t) (label t)

-- Tuple Index ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--
data Graph = G
  { relations :: Map Label (Set Tuple)
  , index :: Map TPattern (Set Tuple)
  }

emptyGraph = (G M.empty M.empty)

projections :: Tuple -> [(TPattern, Tuple)]
projections t | [] <- nodes t = []
projections t@(T{nodes=_:_:_}) =
  [ (TP1 (label t) 0 (nodes t !! 0), t)
  , (TP1 (label t) 1 (nodes t !! 1), t)
  ]
projections t = [(TP1 (label t) 0 (nodes t !! 0), t)]

insertTuple :: Tuple -> Graph -> Graph
insertTuple t (G m i) = G
  { relations = M.insertWith (S.union) (label t) (S.singleton t) m
  , index = foldr (\(p, t) -> M.insertWith (S.union) p (S.singleton t)) i (projections t)
  }

removeTuple :: Tuple -> Graph -> Graph
removeTuple t (G m i) = G
  { relations = M.adjust (S.delete t) (label t) m
  , index = foldr (\(p, t) -> M.adjust (S.delete t) p) i (projections t)
  }

toGraph :: [Tuple] -> Graph
toGraph = foldr step emptyGraph
  where
    step t = insertTuple t

fromGraph :: Graph -> [Tuple]
fromGraph = concat . map (S.toList . snd) . M.toList . relations

constrainRelation :: Label -> Graph -> [Tuple]
constrainRelation l (G g _) =
  case M.lookup l g of
    Nothing -> []
    Just x -> S.toList x

data TPattern = TP1 Label Int Node
  deriving (Eq, Show, Ord)

m2l Nothing = []
m2l (Just x) = x

constrainRelation1 :: TPattern -> Graph -> [Tuple]
constrainRelation1 t@(TP1 l _ _) (G g i) = m2l (S.toList <$> M.lookup t i)
-- ~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--

type Name = String

data NodeVar = NVal Node | NVar Name | NHole
  deriving (Eq, Show, Ord)

instance IsString NodeVar where
  fromString = NVar

data Op = QEq | QDisEq | QLess | QMore | QLessEq | QMoreEq
  deriving (Eq, Show, Ord)

data Dot = High | Low
  deriving (Eq, Show, Ord)
data Linear = Linear | NonLinear
  deriving (Eq, Show, Ord)

data EP
  = EP Linear Label [NodeVar]
  | LP Polarity Label [NodeVar]
  deriving (Eq, Show, Ord)

data NumOp = Sum | Mul | Sub
  deriving (Eq, Show, Ord)

data E = EBinOp NumOp E E
       -- TODO combine with above
       | EConcat E E
       | ELit Int
       | EVar Name
       | ENamed String
       | EString String
       | EHole
  deriving (Eq, Show, Ord)

instance IsString E where
  fromString = EVar
instance Num E where
  fromInteger = ELit . fromInteger
  (+) = EBinOp Sum
  (*) = EBinOp Mul
  (-) = EBinOp Sub
  abs _ = error "abs not implemented for Num E"
  signum  _ = error "signum not implemented for Num E"

-- Left-hand side of rule
data Query =
  -- nb: the ordering of these constructors is significant
  --     (S.toAscList in getMatches in Graph)
  --   TODO don't rely on this
  Query Dot EP
  | QBinOp Op E E
  -- TODO rand
  deriving (Eq, Show, Ord)

-- Right-hand side of rule
data Assert =
  Assert Label [E]
  deriving (Eq, Show, Ord)

assertRel (Assert rel _) = rel

type LHS = [Query]
type RHS = [Assert]

data RType = Event | View
  deriving (Eq, Show, Ord)

data Rule
  = Rule  { rtype :: RType, lhs :: LHS, rhs :: RHS }
  -- | LRule { lhs :: LHS, rhs :: RHS }
  deriving (Eq, Show, Ord)

nullRule :: Rule
nullRule = Rule Event [] []

lhsRule (Rule _ r _) = r
--lhsRule (LRule r _) = r

type Signature = (Label, Maybe Id)
type Pattern = Set Query
type Trigger = (Linear, Rule, Query, Pattern)
type Index = Map Signature [Trigger]
emptyIndex = M.empty

type Context = [(Name, Node)]
type Matched = [Tuple]
--type Matched = [Event]
type Bindings = (Context, Consumed, Matched)
emptyMatchBindings = ([], [], [])

type Match = (Provenance, Context)

data Msg = MT Polarity Tuple
  deriving (Eq, Show, Ord)

-- Utilities
first f (a, b) = (f a, b)
second f (a, b) = (a, f b)

clean :: Ord k => Map k [v] -> Map k [v]
clean = M.filter (not . null)

look k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> error $ "missing key: " ++ show k

lookDefault :: (Ord k, Monoid m) => k -> Map k m -> m
lookDefault k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> mempty

-- Accessors
-- TODO reorg Event/Msg/Tuple/Fact types?
--etuple (E _ t) = t
--etuple _ = error "etuple expects E Event"
--elabel :: Event -> Label
--elabel (E _ t) = label t
--elabel (EFact (l, _) _) = l
--elabel (EFalse (l, _)) = l
--efact :: Event -> Fact
--efact e = (elabel e, enodes e)
--enodes :: Event -> [Node]
--enodes (E _ t) = nodes t
--enodes (EFact (_, ns) _) = ns
--enodes (EFalse (_, ns)) = ns
--epolarity (E p _) = p
--epolarity (EFact _ _) = Positive
--epolarity (EFalse _) = Negative

tpolarity t =
  case tval t of
    t@(Truth _) -> Just t
    _ -> Nothing

mprov (MT _ t) = source t
--mprov (MF _ _ p) = p
mlabel (MT _ t) = label t
--mlabel (MF _ (l, _) _) = l
mfact (MT _ t) = (label t, nodes t)
isPositive (MT Positive _) = True
isPositive _ = False
--mfact (MF _ f _) = f

epLabel :: EP -> Label
epLabel (EP _ l _) = l
epLabel (LP _ l _) = l
epSign EP{} = Nothing
epSign (LP Positive _ _) = Just (Truth True)
epSign (LP Negative _ _) = Just (Truth False)
epNodes :: EP -> [NodeVar]
epNodes (EP _ _ ns) = ns
epNodes (LP _ _ ns) = ns
epLinear (EP l _ _) = l
epLinear _ = NonLinear

ppFact (l, ns) = unwords $ [show l] ++ map show ns
ppTuple (T{..}) = show tval++":"++ppFact (label, nodes)
--ppEvent (E Positive t) = "+"++ppTuple t
--ppEvent (E Negative t) = "-"++ppTuple t
--ppEvent (EFact f ps) = "(+"++show (length ps)++")"++ppFact f
--ppEvent (EFalse f) = "-"++ppFact f
--ppEvents = intercalate ", " . map ppEvent
ppEvent = ppTupleProv
ppMatch :: Provenance -> String
ppMatch (Provenance{..}) = "["++maybe "" ppTuple tuple_src ++"] "++intercalate "," (map ppTuple matched)
ppMatch (Reduction Or r) = "\\/"++"["++(unwords $ map ppTuple r)++"]"
ppMatch (Extern ids) = "[EXTERN: "++show ids++"]"
ppMsg :: Msg -> String
ppMsg (MT Positive t) = "+"++ppEvent t
ppMsg (MT Negative t) = "-"++ppEvent t
--ppMsg (MF Positive f pr) = "+"++ppFact f++"<~"++ppMatch pr
--ppMsg (MF Negative f pr) = "-"++ppFact f++"<~"++ppMatch pr
ppTupleProv (T{..}) = show tval++":"++ppFact (label, nodes)++"{"++ppMatch source++"}"
