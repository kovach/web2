{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module REPL where

import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Monoid()
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Types
import Parser
import Rules
import Graph
import Monad
import Reflection

step1 :: [Rule] -> Graph -> [Tuple]
step1 rules g = ts
  where
    rels = nub . concatMap lhsRels $ rules
    ts = S.toList $ mconcat $ M.elems $ subg
    subg = M.filterWithKey (\k _ -> k `elem` rels) $ relations g

data Action = AQuery LHS | ARule (Rule, String)
  deriving (Show)

data Out = OQuery [Context] | ORule [Msg]

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left s) = Left (f s)
mapLeft _ (Right x) = Right x

parseString p s = do
  ls <- lexLine s
  mapLeft fst $ runParser p ls

newRule :: LineRule -> SM RuleId
newRule (_, rule, str) = do
  i <- lift freshNode
  modify $ \s -> s { rule_map = M.insert i (rule, str) (rule_map s) }
  return i

loadProgram :: String -> String -> SM ()
loadProgram name str =
  case parseRuleFile str of
    Right rs -> do
      ids <- mapM newRule rs
      modify $ \s -> s { program_map = M.insert name ids (program_map s) }
    Left _ -> error $ "Cannot load invalid program: " ++ name


parse :: String -> Either Error Action
parse s = do
  let doError err1 err2 = Left $ unlines ["couldn't parse query or rule:", err1, err2]
  let tryRule err1 =
        case parseString rule_ s of
          Right (r, []) -> Right (ARule (r, s))
          Right _ -> doError err1 ""
          Left err2 -> doError err1 err2
  let tryQuery =
        case parseString lhs_ s of
          Right (q, []) -> Right (AQuery q)
          Right _ -> tryRule ""
          Left err1 -> tryRule err1
  tryQuery

freshActor :: SM Actor
freshActor = lift $ ActorObject <$> freshNode

removeActor :: Actor -> PS -> PS
removeActor act ps@PS{..} = ps
  { dependencies = fmap (filter (/=  act)) dependencies
  , queues = M.delete act queues
  , processors = M.delete act processors
  }

data MetaCommand
  = MakeApp Node String
  | DoReflect Node Int
  -- ?? Don't need this (nor Node in MakeApp)
  | MakeRepl Node
  | DoParse Node String
  | RunReplQuery Node String
  | ChangeRule Node String
  | DeleteRule Node ProgramName
  | AddRule Node ProgramName

commandRelations :: [Label]
commandRelations =
  [ LA "make-app" 2
  , LA "reflect"  2
  , LA "make-repl" 1
  , LA "parse" 2
  , LA "parse-run" 2
  , LA "update-rule" 2
  , LA "delete-rule" 2
  , LA "add-rule" 2
  ]

parseWorkerCommand :: Tuple -> Maybe Node
parseWorkerCommand T {label = LA "new" 2, nodes = [n1@(NNode _)] } = Just n1
parseWorkerCommand _ = Nothing

parseMetaCommand :: Tuple -> Maybe MetaCommand
parseMetaCommand T {label = LA "make-app" 2, nodes = [n1@(NNode _), n2@(NString name)] } =
  Just $ MakeApp n1 name
parseMetaCommand T {label = LA "reflect" 2, nodes = [n1@(NNode _), n2@(NNode tid)] } =
  Just $ DoReflect n1 tid
parseMetaCommand T {label = LA "make-repl" 1, nodes = [n1@(NNode _)] } =
  Just $ MakeRepl n1
parseMetaCommand T {label = LA "parse" 2, nodes = [n1@(NNode _), n2@(NString str)] } =
  Just $ DoParse n1 str
parseMetaCommand T {label = LA "parse-run" 2, nodes = [n1@(NNode _), n2@(NString str)] } =
  Just $ RunReplQuery n1 str
parseMetaCommand T {label = LA "update-rule" 2, nodes = [n1@(NNode _), n2@(NString str)] } =
  Just $ ChangeRule n1 str
parseMetaCommand T {label = LA "delete-rule" 2, nodes = [n1@(NNode _), n2@(NString name)] } =
  Just $ DeleteRule n1 name
parseMetaCommand T {label = LA "add-rule" 2, nodes = [n1@(NNode _), n2@(NString name)] } =
  Just $ AddRule n1 name

parseMetaCommand _ = Nothing

setEnv :: PS -> SM ()
setEnv e = modify $ \ss -> ss { environment = e }
