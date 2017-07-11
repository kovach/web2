-- Possible Language Extensions --
--
-- replace "." with span concept
--   LHS split into a sequence of spans
--   given a match up to some span S, a match for S must include only strictly older tuples
--   so [.p x, q x, r x y] becomes [p x | q x, r x y]
--   more expressive; allows patterns like [p n | q x y | p (n-1)]
--   indexing: triggers for Tuples (EP) in the first span, and Facts (LP) in any span
--
-- scoped relations
--   when combining blocks of code, need to control sharing
--   might want to hide/rename tuples
--   might compartmentalize mutation
--     blocks see distinct copies of an event; .. consumes local copy
--   ? might want to separately rename tuple patterns based on side of rule they appear on
--     allow new rules to mediate between the two
--
-- ? different namespaces for relations with different arity
--
-- lazy tuples
--   ability to mark a particular rhs assertion
--   marked tuples are not recognized by any pattern until all unmarked tuples are processed
--
-- monoids
--   ? do this next
--   Update maintains a set of proofs for a given fact
--   matches only observe the overall truth value
--   can model this as a map into Bool followed by Or
--     could implement other reductions, like (map into Int, +);
--       then the maintenance system in Update becomes an incremental function evaluator
--
-- syntax: block patterns
--   allow a lhs/rhs to be split up across multiple lines when enclosed by [ ]
--   could (sort of) replace .graph file with a series of ` => [...]` rules in rule file

-- TODO reorg Event/Msg/Tuple/Fact types?
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Maybe (fromJust)
import Data.String
import Data.List (intercalate, delete)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace

newtype Label = L String
  deriving (Eq, Ord)

instance IsString Label where
  fromString = L
instance Show Label where
  show (L s) = s

nullLabel = L ""

-- TODO rename constructors
data Node = NInt Int | NNode Int | NSymbol String | NString String
  deriving (Eq, Ord)

type Id = Int

data Polarity = Positive | Negative
  deriving (Eq, Show, Ord)
neg :: Polarity -> Polarity
neg Positive = Negative
neg Negative = Positive

data Event = E Polarity Tuple
           | EFact Fact [Provenance]
           | EFalse Fact
  deriving (Show)

instance Eq Event where
  (E p1 t1) == (E p2 t2) = p1 == p2 && t1 == t2
  (EFact f1 _) == (EFact f2 _) = f1 == f2
  (EFalse f1) == (EFalse f2) = f1 == f2
  _ == _ = False

instance Ord Event where
  e1 `compare` e2 = e1' `compare` e2'
    where
      e1' = case e1 of
              EFact f _ -> EFact f []
              _ -> e1
      e2' = case e2 of
              EFact f _ -> EFact f []
              _ -> e2

etuple (E _ t) = t
etuple _ = error "etuple expects E Event"
elabel :: Event -> Label
elabel (E _ t) = label t
elabel (EFact (l, _) _) = l
elabel (EFalse (l, _)) = l
efact :: Event -> Fact
efact e = (elabel e, enodes e)
enodes :: Event -> [Node]
enodes (E _ t) = nodes t
enodes (EFact (_, ns) _) = ns
enodes (EFalse (_, ns)) = ns
epolarity (E p _) = p
epolarity (EFact _ _) = Positive
epolarity (EFalse _) = Negative

toEvent :: Tuple -> Event
toEvent = E Positive

type Dependency = [Event]
type Consumed = [Tuple]

-- An instance of a match
-- TODO include context bindings?
data Provenance = Provenance
  -- The rule of this match
  { rule_src :: Rule
  -- The tuple that triggered this match instance
  -- Nothing for rules with empty LHS, or external inputs
  , tuple_src :: Maybe Event
  -- Tuples matched by this match instance
  , matched :: Dependency
  -- Tuples removed from the world by this match instance
  , consumed :: Consumed
  }
  | Extern [Int] -- TODO ??
  deriving (Eq, Show, Ord)

nullProv :: Provenance
nullProv = Provenance nullRule Nothing [] []

ppFact (l, ns) = unwords $ [show l] ++ map show ns
ppTuple (T{..}) = show tid++":"++ppFact (label, nodes)
ppEvent (E Positive t) = "+"++ppTuple t
ppEvent (E Negative t) = "-"++ppTuple t
ppEvent (EFact f ps) = "(+"++show (length ps)++")"++ppFact f
ppEvent (EFalse f) = "-"++ppFact f
ppEvents = intercalate ", " . map ppEvent

ppMatch :: Provenance -> String
ppMatch (Provenance{..}) = "["++maybe "" ppEvent tuple_src ++"] "++ppEvents matched
ppMatch (Extern ids) = "[EXTERN: "++show ids++"]"

type RawTuple = (Label, [Node])
type Fact = RawTuple

data Tuple
  = T
  { nodes :: [Node]
  , label :: Label
  , source :: Provenance
  , tid :: Id }
  deriving (Show)

instance Eq Tuple where
  t1 == t2 = tid t1 == tid t2

instance Ord Tuple where
  t1 `compare` t2 = tid t1 `compare` tid t2

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
data Unique = Unique | NonUnique
  deriving (Eq, Show, Ord)
-- TODO remove/fix Unique feature
-- currently disabled in parser and matcher
data EP
  = EP Linear Unique Label [NodeVar]
  | LP Polarity Label [NodeVar]
  deriving (Eq, Show, Ord)

epLabel :: EP -> Label
epLabel (EP _ _ l _) = l
epLabel (LP _ l _) = l

epSign EP{} = Positive
epSign (LP p _ _) = p

epNodes :: EP -> [NodeVar]
epNodes (EP _ _ _ ns) = ns
epNodes (LP _ _ ns) = ns

epLinear (EP l _ _ _) = l
epLinear _ = NonLinear

data NumOp = Sum | Mul | Sub
  deriving (Eq, Show, Ord)

data E = EBinOp NumOp E E
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

data Rule
  = Rule  { lhs :: LHS, rhs :: RHS }
  | LRule { lhs :: LHS, rhs :: RHS }
  deriving (Eq, Show, Ord)

nullRule :: Rule
nullRule = Rule [] []

rhsRule (Rule _ r) = r
rhsRule (LRule _ r) = r

type Signature = (Label, Polarity)
type Pattern = Set Query
type Trigger = (Linear, Rule, Query, Pattern)
type Index = Map Signature [Trigger]
emptyIndex = M.empty

type Context = [(Name, Node)]
type Matched = [Event]
type Bindings = (Context, Consumed, Matched)
emptyMatchBindings = ([], [], [])

type Match = (Provenance, Context)

-- TODO remove NULL?
--   or change msgLog type in Monad
data Msg = MT Polarity Tuple | MF Polarity Fact Provenance
  deriving (Eq, Show, Ord)

ppMsg :: Msg -> String
ppMsg (MT p t) = ppEvent (E p t)
ppMsg (MF Positive f pr) = "+"++ppFact f++"<~"++ppMatch pr
ppMsg (MF Negative f pr) = "-"++ppFact f++"<~"++ppMatch pr

mprov (MT _ t) = source t
mprov (MF _ _ p) = p

mlabel (MT _ t) = label t
mlabel (MF _ (l, _) _) = l

mfact (MT _ t) = (label t, nodes t)
mfact (MF _ f _) = f

-- Utilities
pad n s = s ++ replicate (n - length s) ' '

first f (a, b) = (f a, b)
second f (a, b) = (a, f b)

clean :: Ord k => Map k [v] -> Map k [v]
clean = M.filter (not . null)

diffList :: Ord a => [a] -> [a] -> [a]
diffList a b = S.toList (S.fromList a \\ S.fromList b)

look k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> error $ "missing key: " ++ show k

lookList :: Ord k => k -> Map k [v] -> [v]
lookList k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> []
