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
import Update
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
  deriving (Show)

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



-- TODO refactor this into Graph?
step2 :: Rule -> Tuple -> (Graph, [Context]) -> (Graph, [Context])
step2 rule t (g, out) = (g2, cs ++ out)
  where
    matches = map fix $ getMatches t (unsafeRanked 0 rule) g
    cs :: [Context]
    cs = map snd matches
    removed = concatMap fst matches
    g2 = foldr removeTuple (insertTuple t g) removed
    fix (p,c,_) = (consumed p, c)

eval :: Action -> Graph -> M2 Out
eval (ARule (rule, _)) g = do
  NNode rid <- freshNode
  let ts = step1 [rule] g
  (output, _) <- stepRule (MQ { m_pos = ts, m_neg = [] }) (RankedRule rid rule) g
  return (ORule output)
eval (AQuery q) g = do
  let rule = Rule Event q [] -- fake rule
  let ts = step1 [rule] g
      (_, cs) = foldr (step2 rule) (g, []) ts
  return (OQuery cs)

--
-- Terrible Hacks --
--
--
freshActor = lift $ ActorObject <$> freshNode

pushActor :: Actor -> [Msg] -> PS -> PS
pushActor act ms ps = ps
  { queues = M.adjust (\q -> foldr pushQueue q ms) act (queues ps) }

newProgramProc :: ProgramName -> SM ()
newProgramProc name = do
  SS{program_map, rule_map} <- get
  let ruleIds = look name program_map
      rules = map (\i -> fst $ look i rule_map) ruleIds
      watches = inputRelations rules
  actor <- freshActor
  proc <- SubProgram name <$> lift (initPS rules emptyGraph)
  let inputs = M.fromList $ zip watches (repeat [actor])
      fix ps@PS{..} = ps
          { dependencies = M.unionWith (++) dependencies inputs
          , processors = M.insert actor proc processors
          }
  modify $ \ss -> ss { environment = fix (environment ss) }
  where

removeActor :: Actor -> PS -> PS
removeActor act ps@PS{..} = ps
  { dependencies = fmap (filter (/=  act)) dependencies
  , queues = M.delete act queues
  , processors = M.delete act processors
  -- , gas_limits = M.delete act gas_limits
  }

-- System message schema --
--
-- create app-id string
-- reflect container-id [..tid]
-- parse parent str
-- repl app-id
-- change-rule id str
-- delete-rule id ProgramName
-- add-rule    id ProgramName
--
-- reset app-id -- refresh rules/db

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

stepWorker :: MsgQueue -> SM ([Msg], Processor)
stepWorker mq@MQ{m_pos, m_neg} = do
  envPS <- gets environment
  tupleIds <- gets tuple_ids
  (msgs, env') <- solve (toMsgs mq) envPS
  setEnv env'
  -- keep tuples up to date
  let tupleIds1 = foldr (IM.delete . tid) tupleIds m_neg
  let tupleIds2 = foldr (\t -> IM.insert (tid t) t) tupleIds1 m_pos
  modify $ \ss -> ss { tuple_ids = tupleIds2 }
  return (msgs, WorkerProc)

stepCreator :: MsgQueue -> SM ([Msg], Processor)
stepCreator mq@MQ{m_pos, m_neg} = do
    SS{..} <- get
    output <- concat <$> mapM handleCommand commands
    return (output, CreatorProc)
  where
    commands = mapMaybe parseMetaCommand m_pos

    handleCommand (DoReflect target tid) = do
      rc <- gets refl_context
      tlookup <- gets tuple_ids
      let tuple = (fromJust . flip IM.lookup tlookup) tid
      (_, rc') <- lift $ runReflection (flattenTuple tuple) rc
      modify $ \ss -> ss { refl_context = rc' }
      -- TODO better way to get these out
      lift $ flushEvents

    handleCommand (MakeApp _ name) = do
      newProgramProc name
      return []

initMetaPS :: [(ProgramName, String)] -> SM PS
initMetaPS ruleSets = do
    worker  <- freshActor
    creator <- freshActor
    -- sets the program_map
    mapM (uncurry loadProgram) ruleSets

    --rankedRuleSets <- mapM (secondM rankRules) ruleSets
    --(ruleSetPairs, rc) <- runReflection (mapM (secondM fix) rankedRuleSets) emptyReflContext
    --let ruleMap = M.fromList ruleSetPairs
    -- store in state with rc

    let procs = [ (worker, WorkerProc) , (creator, CreatorProc) ]

    return $ PS
      { dependencies = M.empty
      , queues = M.empty
      , processors = M.fromList procs
      }
  where
    fix rs = do
      i <- flattenRules rs
      return rs

todo = do
  let files = [("sieve", "examples/sieve.arrow")]
  strs <- mapM (readFile . snd) files
  let prog = initMetaPS (zip (map fst files) strs)
  return ()

{-

-- TODO
-- IO
--  load several programs
--  create program map for them
--  make SS
--  launch "controller" program
--    click a button, launch that program
-}
