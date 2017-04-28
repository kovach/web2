{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.String
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

data Time = Time [Int]
  deriving (Eq, Show, Ord)
appT (Time a) (Time b) = Time (reverse b ++ a) -- TODO
newtype Label = L String
  deriving (Eq, Ord)

instance IsString Label where
  fromString = L
instance Show Label where
  show (L s) = s

data Tuple = T
  { nodes :: [Node]
  , label :: Label
  , ts :: Time }
  deriving (Eq, Show, Ord)

-- for now, objects (indexed by int) or literal ints
data Node = NTInt Int | NTRef Int | NTNamed String
  deriving (Eq, Ord)

instance IsString Node where
  fromString = NTNamed

instance Num Node where
  fromInteger = NTInt . fromInteger

instance Show Node where
  show (NTInt i) = show i
  show (NTRef i) = "#"++show i
  show (NTNamed s) = "'"++s

type Count = Int

data DB = DB
  { tuples :: [Tuple]
  , time_counter :: Int
  , id_counter :: Count
  }
  deriving (Eq, Show, Ord)

data DBUpdate = DBU
  { new_tuples :: [Tuple]
  , new_id_counter :: Count
  }

initDB g = DB { tuples = g, time_counter = 0, id_counter = 0 }
emptyDB = initDB []

type Name = String
--type Edge = (Label, (Node, Node))
type Edge = Tuple

data Q = QVal Node | QVar Name
  deriving (Eq, Show, Ord)

instance IsString Q where
  fromString = QVar
instance Num Q where
  fromInteger = QVal . fromInteger

data NumOp = Sum | Mul | Sub
  deriving (Eq, Show, Ord)
data E = EBinOp NumOp E E | ELit Int | EVar Name | ENamed String
  deriving (Eq, Show, Ord)

instance IsString E where
  fromString = EVar

instance Num E where
  fromInteger = ELit . fromInteger
  (+) = EBinOp Sum
  (*) = EBinOp Mul
  (-) = EBinOp Sub

data EP =
  EP Label [Q]
  -- | Tuple Label [Q]
  deriving (Eq, Show, Ord)

data Op = QEq | QDisEq | QLess | QMore
  deriving (Eq, Show, Ord)

-- Left-hand side of rule
data Query =
  Query EP
  | DotQuery EP
  | HashQuery EP
  | Counter [String] [Query]
  | QBinOp Op Q Q
  -- TODO forall/unique/some/empty
  -- rand/single
  deriving (Eq, Show, Ord)


-- Right-hand side of rule
data Assert =
  Assert Label [E]
  deriving (Eq, Show, Ord)

type Pattern = Set Query
type LHS = [Query]
type RHS = [Assert]

type Trigger = (Rule, EP, Pattern)
type Index = Map Label [Trigger]

data Rule = Rule LHS RHS
  deriving (Eq, Show, Ord)

type Graph = [Tuple]

type Context = [(Name, Node)]

type Match = (Context, RHS)

-- Utilities

-- Dumb version of unfold
unfold :: (a -> Maybe a) -> a -> [a]
unfold f x =
  case f x of
    Nothing -> [x] -- unfoldr returns []
    Just x' -> x : unfold f x'
