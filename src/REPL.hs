{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module REPL where

import Data.List
import Data.Maybe
import Control.Monad.State
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

import Types
import Parse
import Parser
import Rules
import Graph
import Monad
import Reflection

newRule :: LineRule -> SM RuleId
newRule (_, rule) = do
  i <- lift freshNode
  modify $ \s -> s { rule_map = M.insert i rule (rule_map s) }
  return i

-- TODO reorg rule parsing
newProgram :: String -> String -> SM Bool
newProgram name str =
  case parseRuleFile str of
    Right rs -> do
      ids <- mapM newRule rs
      modify $ \s -> s { program_map = M.insert name ids (program_map s) }
      return True
    _ -> return False
    --Left _ -> error $ "Cannot load invalid program: " ++ name

data Action = AQuery LHS | ARule Rule
  deriving (Show)

data Out = OQuery [Context] | ORule [Msg]

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left s) = Left (f s)
mapLeft _ (Right x) = Right x

parseString p s = do
  ls <- lexLine s
  mapLeft fst $ runParser p ls

fullParse p s =
  case parseString p s of
    Right (v, []) -> Right v
    Right _ -> Left "incomplete parse"
    Left err -> Left err

parseRule :: String -> Either Error Rule
parseRule s =
  case parseString line_ s of
    Right (v, []) -> Right v
    Right _ -> Left "incomplete parse"
    Left err -> Left err

replParse :: String -> Either Error LHS
replParse s = do
  lhs <- fullParse lhs_ s
  return (labelLHSArity lhs)

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
  | DoReflect Id Node
  | Attributes Node Node
  | EditRule Node
  | MakeRepl Node
  | DoParse Node String
  | RunReplQuery Node String
  | ChangeRule Node String
  | DeleteRule Node ProgramName
  | AddRule Node ProgramName
  | DoRefl

commandRelations :: [Label]
commandRelations =
  [ LA "make-app" 2
  , LA "reflect"  2
  , LA "attributes" 2
  , LA "edit-rule" 1
  , LA "update-rule" 2
  -- TODO
  , LA "make-repl" 1
  , LA "io/parse-rule" 2
  , LA "io/run-query" 2
  , LA "delete-rule" 2
  , LA "add-rule" 2
  , LA "refl" 1
  ]

parseMetaCommand :: Tuple -> Maybe MetaCommand
parseMetaCommand T {label = LA "make-app" 2, nodes = [n1@(NNode _), n2@(NString name)] } =
  Just $ MakeApp n1 name
parseMetaCommand T {label = LA "reflect" 2, nodes = [n1@(NNode tid), n2@(NNode _)] } =
  Just $ DoReflect tid n2
parseMetaCommand T {label = LA "attributes" 2, nodes = [n1@(NNode _), n2@(NNode _)] } =
  Just $ Attributes n1 n2
parseMetaCommand T {label = LA "edit-rule" 1, nodes = [n1@(NNode _)] } =
  Just $ EditRule n1
parseMetaCommand T {label = LA "update-rule" 2, nodes = [n1@(NNode _), n2@(NString str)]} =
  Just $ ChangeRule n1 str
parseMetaCommand T {label = LA "make-repl" 1, nodes = [n1@(NNode _)] } =
  Just $ MakeRepl n1
parseMetaCommand T {label = LA "io/parse-rule" 2, nodes = [n1@(NNode _), n2@(NString str)] } =
  Just $ DoParse n1 str
parseMetaCommand T {label = LA "io/run-query" 2, nodes = [n1@(NNode _), (NString str)] } =
  Just $ RunReplQuery n1 str
parseMetaCommand T {label = LA "delete-rule" 2, nodes = [n1@(NNode _), n2@(NString name)] } =
  Just $ DeleteRule n1 name
parseMetaCommand T {label = LA "add-rule" 2, nodes = [n1@(NNode _), n2@(NString name)] } =
  Just $ AddRule n1 name
parseMetaCommand T {label = LA "refl" 1, tid = tid, nodes = [n1@(NNode _)] } =
  Just $ DoReflect (Id tid) n1

parseMetaCommand _ = Nothing

setEnv :: PS -> SM ()
setEnv e = modify $ \ss -> ss { environment = e }


-- TODO use standard code for this
queryEval :: Graph -> LHS -> M2 ([Msg], [Context])
queryEval g lhs = do
    pairs <- mapM applyMatch matches
    let (deletions, ctxts) = unzip pairs
    return (concat deletions, ctxts)
  where
    rule = (Rule Nothing Nothing Event lhs [])
    rrule = RankedRule 1 rule
    rels = lhsRels rule
    ts = filter (\t -> label t `elem` rels) $ fromGraph g

    (matches, _) = foldl' step ([], emptyGraph) ts
    step (ms, g) t = (getMatches t rrule g ++ ms, insertTuple t g)

--foo str = do
--  ok <- newProgram "query" str
--  if ok
--    then do
--      -- run "query"
--      return ()
--    else do
--      -- TODO return "bad parse"
--      return ()

-- TODO integrate this
--
-- TODO refactor this into Graph?
-- step2 :: Rule -> Tuple -> (Graph, [Context]) -> (Graph, [Context])
-- step2 rule t (g, out) = (g2, cs ++ out)
--   where
--     matches = map fix $ getMatches t (unsafeRanked 0 rule) g
--     cs :: [Context]
--     cs = map snd matches
--     removed = concatMap fst matches
--     g2 = foldr removeTuple (insertTuple t g) removed
--     fix (p,c,_) = (consumed p, c)
--
-- step1 :: [Rule] -> Graph -> [Tuple]
-- step1 rules g = ts
--   where
--     rels = nub . concatMap lhsRels $ rules
--     ts = S.toList $ mconcat $ M.elems $ subg
--     subg = M.filterWithKey (\k _ -> k `elem` rels) $ relations g
--
-- eval :: Action -> Graph -> M2 Out
-- eval (ARule rule) g = do
--   NNode rid <- freshNode
--   let ts = step1 [rule] g
--   (output, _) <- stepRule (MQ { m_pos = ts, m_neg = [] }) (RankedRule rid rule) g
--   return (ORule output)
-- eval (AQuery q) g = do
--   let rule = Rule Event q [] -- fake rule
--   let ts = step1 [rule] g
--       (_, cs) = foldr (step2 rule) (g, []) ts
--   return (OQuery cs)
