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
-- parsing: block patterns
--   allow a lhs/rhs to be split up across multiple lines when enclosed by [ ]
--   could (sort of) replace .graph file with a series of ` => [...]` rules at head of rule file

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Maybe (fromJust)
import Data.String
import Data.List (intercalate, delete)
import Data.Set (Set)
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

-- for now, objects (indexed by int) or literal ints
data Node = NTInt Int | NTRef Int | NTNamed String
  deriving (Eq, Ord)

type Id = Int

data Polarity = Positive | Negative
  deriving (Eq, Show, Ord)
neg :: Polarity -> Polarity
neg Positive = Negative
neg Negative = Positive

-- TODO rename
data Event2 = E Polarity Tuple
            | EFact Fact [Provenance]
            | EFalse Fact
  deriving (Show, Ord)

instance Eq Event2 where
  (E p1 t1) == (E p2 t2) = p1 == p2 && t1 == t2
  (EFact f1 _) == (EFact f2 _) = f1 == f2
  (EFalse f1) == (EFalse f2) = f1 == f2
  _ == _ = False

etuple (E _ t) = t
elabel :: Event2 -> Label
elabel (E _ t) = label t
elabel (EFact (l, _) _) = l
elabel (EFalse (l, _)) = l
enodes :: Event2 -> [Node]
enodes (E _ t) = nodes t
enodes (EFact (_, ns) _) = ns
enodes (EFalse (_, ns)) = ns
epolarity (E p _) = p
epolarity (EFact _ _) = Positive
epolarity (EFalse _) = Negative

toEvent :: Tuple -> Event2
toEvent = E Positive

type Dependency = [Event2]
type Consumed = [Tuple]

-- An instance of a match
data Provenance = Provenance
  -- The rule of this match
  { rule_src :: Rule
  -- The tuple that triggered this match instance
  -- Nothing for rules with empty LHS, or external inputs
  , tuple_src :: Maybe Event2
  -- Tuples matched by this match instance
  , matched :: Dependency
  -- Tuples removed from the world by this match instance
  , consumed :: Consumed
  } deriving (Eq, Show, Ord)


ppFact (l, ns) = unwords $ [show l] ++ map show ns
ppTuple (T{..}) = show tid++":"++ppFact (label, nodes)
ppEvent (E Positive t) = "+"++ppTuple t
ppEvent (E Negative t) = "-"++ppTuple t
ppEvent (EFact f ps) = "(+"++show (length ps)++")"++ppFact f
ppEvent (EFalse f) = "-"++ppFact f
ppEvents = intercalate ", " . map ppEvent

ppFS :: FactState -> String
ppFS = unlines . map pp . M.toList
  where
    pp (f, ps) = unlines $ [show f ++ " : " ++ show (length ps)] ++ map (("  " ++) . ppMatch) ps

ppMatch :: Provenance -> String
ppMatch (Provenance{..}) = "("++maybe "" ppEvent tuple_src ++") "++ppEvents matched

type RawTuple = (Label, [Node])
type Fact = RawTuple

data Tuple
  = T
  { nodes :: [Node]
  , label :: Label
  , source :: Provenance
  , tid :: Id }
  deriving (Show, Ord)

instance Eq Tuple where
  t1 == t2 = tid t1 == tid t2

instance IsString Node where
  fromString = NTNamed

instance Show Node where
  show (NTInt i) = show i
  show (NTRef i) = "#"++show i
  show (NTNamed s) = "'"++s

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

type FactState = Map Fact [Provenance]

emptyFS :: FactState
emptyFS = M.empty

data DB = DB
  { tuples :: Graph
  , facts :: FactState
  , removed_tuples :: [Tuple] -- TODO remove?
  , node_counter :: Count
  , tuple_counter :: Count
  }
  deriving (Eq, Show, Ord)

initDB g = DB { tuples = toGraph g, facts = emptyFS
              , removed_tuples = []
              , node_counter = 0, tuple_counter = 0}
emptyDB = initDB []

allTuples :: DB -> [Tuple]
allTuples db = fromGraph (tuples db) ++ removed_tuples db

type Name = String

data NodeVar = NVal Node | NVar Name | NHole
  deriving (Eq, Show, Ord)

instance IsString NodeVar where
  fromString = NVar

data Op = QEq | QDisEq | QLess | QMore
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
       | EHole
  deriving (Eq, Show, Ord)
instance IsString E where
  fromString = EVar
instance Num E where
  fromInteger = ELit . fromInteger
  (+) = EBinOp Sum
  (*) = EBinOp Mul
  (-) = EBinOp Sub

-- Left-hand side of rule
data Query =
  Query Dot EP
  -- | Counter [NodeVar] [Query] -- TODO implement?
  -- nb: the ordering of these constructors is significant
  --   TODO don't rely on this
  | QBinOp Op E E
  -- TODO forall/unique/some/empty?
  --      rand
  deriving (Eq, Show, Ord)

-- Right-hand side of rule
data Assert =
  Assert Label [E]
  deriving (Eq, Show, Ord)

assertRel (Assert rel _) = rel

type LHS = [Query]
type RHS = [Assert]

--type RuleId = Int
data Rule
  = Rule { lhs :: LHS, rhs :: RHS }
  | LRule { lhs :: LHS, rhs :: RHS }
  deriving (Eq, Show, Ord)

nullRule :: Rule
nullRule = Rule [] []

rhsRule (Rule _ r) = r
rhsRule (LRule _ r) = r

type Signature = (Label, Polarity)
type Pattern = Set Query
type Trigger = (Linear, Rule, EP, Pattern)
type Index = Map Signature [Trigger]
emptyIndex = M.empty


type Context = [(Name, Node)]
type Matched = [Event2]
type Bindings = (Context, Consumed, Matched)
emptyMatchBindings = ([], [], [])

type Match = (Provenance, Context)

-- TODO remove NULL?
--   or change msgLog type in Monad
data Msg = MT Polarity Tuple | MF Polarity Fact Provenance | NULL (Maybe Rule)
  deriving (Eq, Show, Ord)

ppMsg :: Msg -> String
ppMsg (MT p t) = ppEvent (E p t)
ppMsg (MF Positive f pr) = "+"++ppFact f++"<~"++ppMatch pr
ppMsg (MF Negative f pr) = "-"++ppFact f++"<~"++ppMatch pr
ppMsg (NULL mr) = "\n---\n"++show mr ++"\n---"

mprov (MT _ t) = source t
mprov (MF _ _ p) = p

mlabel (MT _ t) = label t
mlabel (MF _ (l, _) _) = l

-- Utilities
pad n s = s ++ replicate (n - length s) ' '

first f (a, b) = (f a, b)
second f (a, b) = (a, f b)

toEvents :: FactState -> [Event2]
toEvents = map (uncurry EFact) . M.toList
