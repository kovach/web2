{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.String
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

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

-- the rule, the triggering tuple, all matched tuples
data Provenance = Provenance
  { rule_src :: RuleId
  , tuple_src :: Tuple
  , deps :: Dependency
  } deriving (Eq, Show, Ord)

type RawTuple = (Label, [Node])

data Tuple = T
  { nodes :: [Node]
  , label :: Label
  , tid :: Id
  , source :: Maybe Provenance -- TODO something with this
  }
  deriving (Eq, Show, Ord)

instance IsString Node where
  fromString = NTNamed

instance Num Node where
  fromInteger = NTInt . fromInteger

instance Show Node where
  show (NTInt i) = show i
  show (NTRef i) = "#"++show i
  show (NTNamed s) = "'"++s

type Count = Int

type Graph = Map Label [Tuple]

insertTuple :: Tuple -> Graph -> Graph
insertTuple t = M.insertWith (++) (label t) [t]

toGraph :: [Tuple] -> Graph
toGraph = foldr step M.empty
  where
    step t = insertTuple t

fromGraph :: Graph -> [Tuple]
fromGraph = concat . map snd . M.toList

data DB = DB
  { tuples :: Graph
  , removed_tuples :: [Tuple]
  , node_counter :: Count
  , tuple_counter :: Count
  }
  deriving (Eq, Show, Ord)

initDB g = DB { tuples = toGraph g, removed_tuples = []
              , node_counter = 0, tuple_counter = 0}
emptyDB = initDB []

allTuples :: DB -> [Tuple]
allTuples db = fromGraph (tuples db) ++ removed_tuples db

type Name = String

data NodeVar = NVal Node | NVar Name | NHole
  deriving (Eq, Show, Ord)

instance IsString NodeVar where
  fromString = NVar
instance Num NodeVar where
  fromInteger = NVal . fromInteger

data Op = QEq | QDisEq | QLess | QMore
  deriving (Eq, Show, Ord)

data Dot = High | Low
  deriving (Eq, Show, Ord)
data Linear = Linear | NonLinear
  deriving (Eq, Show, Ord)
data Unique = Unique | NonUnique
  deriving (Eq, Show, Ord)

data EP =
  EP Linear Unique Label [NodeVar]
  deriving (Eq, Show, Ord)

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
  | Counter [NodeVar] [Query] -- TODO implement
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

type Pattern = Set Query
type LHS = [Query]
type RHS = [Assert]

type RuleId = Int
data Rule = Rule LHS RHS
  deriving (Eq, Show, Ord)

type Trigger = (RuleId, Linear, Rule, EP, Pattern)
type Index = Map Label [Trigger]
emptyIndex = M.empty


type Context = [(Name, Node)]
type Consumed = [Tuple]
type Dependency = [Tuple]
type Bindings = (Context, Consumed, Dependency)
emptyMatchBindings = ([], [], [])

type Match = (RuleId, Bindings, RHS)

-- TODO use better data type
takeConsumed :: Match -> Consumed
takeConsumed (_,(_,c,_),_) = c

-- Utilities

-- Dumb version of unfold
unfold :: (a -> Maybe a) -> a -> [a]
unfold f x =
  case f x of
    Nothing -> [x] -- unfoldr returns []
    Just x' -> x : unfold f x'


pad n s = s ++ replicate (n - length s) ' '
ppTuple (T{..}) = unwords $ [pad 0 $ show label] ++ map show nodes
--ppTuple (T{..}) = unwords $ [show tid, show label] ++ map show nodes
