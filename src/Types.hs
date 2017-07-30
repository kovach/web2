-- Language notes in docs/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.String
import Data.List (intercalate, delete)
import Data.Set (Set)
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

--data Event = E Polarity Tuple
--           | EFact Fact [Provenance]
--           | EFalse Fact
--  deriving (Show)

--instance Eq Event where
--  (E p1 t1) == (E p2 t2) = p1 == p2 && t1 == t2
--  (EFact f1 _) == (EFact f2 _) = f1 == f2
--  (EFalse f1) == (EFalse f2) = f1 == f2
--  _ == _ = False
--
--instance Ord Event where
--  e1 `compare` e2 = e1' `compare` e2'
--    where
--      e1' = case e1 of
--              EFact f _ -> EFact f []
--              _ -> e1
--      e2' = case e2 of
--              EFact f _ -> EFact f []
--              _ -> e2
--
-- TODO remove
--toEvent :: Tuple -> Event
--toEvent t = E Positive
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
      (Id i1, Id i2) -> i1 `compare` i2
      (v1, v2) -> (tfact t1, v1) `compare` (tfact t2, v2)

instance IsString Node where
  fromString = NSymbol

instance Show Node where
  show (NInt i) = show i
  show (NNode i) = "#"++show i
  show (NSymbol s) = "'"++s
  show (NString s) = show s

type Count = Int

type Graph = Map Label [Tuple]

insertTuple :: Tuple -> Graph -> Graph
insertTuple t = M.insertWith (++) (label t) [t]

removeTuple :: Tuple -> Graph -> Graph
removeTuple t = M.adjust (delete t) (label t)

toGraph :: [Tuple] -> Graph
toGraph = foldr step M.empty
  where
    step t = insertTuple t

fromGraph :: Graph -> [Tuple]
fromGraph = concat . map snd . M.toList

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

lookList :: Ord k => k -> Map k [v] -> [v]
lookList k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> []

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
