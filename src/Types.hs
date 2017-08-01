-- Language notes in docs/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
data RankedRule = RankedRule {ranked_id :: RuleId, ranked_rule :: Rule }
  deriving (Show)

instance Eq RankedRule where
  RankedRule i1 _ == RankedRule i2 _ = i1 == i2

instance Ord RankedRule where
  RankedRule i1 _ `compare` RankedRule i2 _ = i1 `compare` i2

-- TODO move this into a Monad
rankRules = map (uncurry RankedRule) . (zip [1..])
unsafeRanked = RankedRule

-- TODO
-- sum, argmax (or "most recent"), rand?
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
nullProv = Provenance (RankedRule 0 nullRule) Nothing [] []

externProv = Extern []

type RawTuple = (Label, [Node])
type Fact = RawTuple

data TVal = NoVal | Truth Bool
  deriving (Eq, Ord)

instance Show TVal where
  show NoVal = ""
  show (Truth b) = show b

isPositive = ok . tval
  where
    ok (Truth True) = True
    ok NoVal = True
    ok _ = False

-- TODO add id back; also to Provenance
data Tuple
  = T
  { nodes :: [Node]
  , label :: Label
  , source :: Provenance
  -- TODO really test this
  , tid :: {-# UNPACK #-} !Int
  , tval :: TVal }
  deriving (Show)

tfact :: Tuple -> Fact
tfact t = (label t, nodes t)

isEventTuple T{tval = NoVal} = True
isEventTuple _ = False

instance Eq Tuple where
  {-# INLINE (==) #-}
  t1 == t2 = tid t1 == tid t2

instance Ord Tuple where
  {-# INLINE compare #-}
  t1 `compare` t2 = tid t1 `compare` tid t2

instance IsString Node where
  fromString = NSymbol

instance Show Node where
  show (NInt i) = show i
  show (NNode i) = "#"++show i
  show (NSymbol s) = "'"++s
  show (NString s) = show s

type Count = Int

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
fromGraph = concat . map (S.toAscList . snd) . M.toList . relations

constrainRelation :: Label -> Graph -> [Tuple]
constrainRelation l (G g _) =
  case M.lookup l g of
    Nothing -> []
    Just x -> S.toAscList x

data TPattern = TP1 Label Int Node
  deriving (Eq, Show, Ord)

m2l Nothing = []
m2l (Just x) = x

constrainRelation1 :: TPattern -> Graph -> [Tuple]
constrainRelation1 t@(TP1 l _ _) (G g i) = m2l (S.toAscList <$> M.lookup t i)
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
  deriving (Eq, Show, Ord)

nullRule :: Rule
nullRule = Rule Event [] []

lhsRule (Rule _ r _) = r

type Signature = (Label, Maybe TVal)
type Pattern = Set Query
type Trigger = (Linear, Rule, Query, Pattern)
type Index = Map Signature [Trigger]
emptyIndex = M.empty

type Context = [(Name, Node)]
type Matched = [Tuple]
type Falsified = [(Fact, TVal)]
--type Matched = [Event]
type Bindings = (Context, Consumed, Matched, Falsified)
emptyMatchBindings = ([], [], [], [])

type Match = (Provenance, Context, Falsified)

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

tpolarity t =
  case tval t of
    t@(Truth _) -> Just t
    _ -> Nothing

mprov (MT _ t) = source t
mlabel (MT _ t) = label t
mfact (MT _ t) = (label t, nodes t)

pattern MPos t = MT Positive t
pattern MNeg t = MT Negative t

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

ppId i = "#"++show i
ppFact (l, ns) = unwords $ [show l] ++ map show ns
ppTuple (T{..}) = ppId tid++":"++show tval++":"++ppFact (label, nodes)
ppEvent = ppTupleProv
ppMatch :: Provenance -> String
--ppMatch (Provenance{..}) = "["++maybe "" ppTuple tuple_src ++"] "++intercalate "," (map ppTuple matched)
ppMatch (Provenance{..}) = intercalate ", " (map ppTuple matched)
ppMatch (Reduction Or r) = "\\/"++"["++(unwords $ map ppTuple r)++"]"
ppMatch (Extern ids) = "[EXTERN: "++show ids++"]"
ppMsg :: Msg -> String
ppMsg (MT Positive t) = "+"++ppEvent t
ppMsg (MT Negative t) = "-"++ppEvent t
ppTupleProv t@(T{..}) = ppTuple t++"{"++ppMatch source++"}"
