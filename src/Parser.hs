-- TODO
--   add more identifier examples to documentation, e.g.
--     x+3 間 что rel/n
--   and clarify how variables and unification work in section on patterns
module Parser
  (rquery_, isComment
  , LineRule, parseRuleFile
  , parseTupleFile
  ) where

import Data.Char (isSpace)

import Types hiding (T)
import Parse hiding (string, identifier, char, token, int_)

data Lex a = Token a | Comment a | String a
  deriving (Show, Eq, Ord)

-- TODO use Text?
type L = Lex String

type LParser = ParseEither [L] Error

type LineRule = (Int, Rule)

isComment (Comment _) = True
isComment _ = False

forbidden_characters :: String
forbidden_characters = " \t\n,.;@()[]{}!"
-- ... and \" implicitly

forbidden_identifier = ["~>", "=>", ",", "!"]

spaceout :: [L] -> [L]
spaceout = concatMap fm
  where
    split p = words . concatMap fix
      where
        fix c | p c = [' ', c, ' ']
        fix c = [c]
    fm (Token s) = map Token . split (`elem` forbidden_characters) $ s
    fm v = [v]

lexLine :: String -> Either Error [L]
lexLine str = fix [] empty str
  where
    empty = []
    emit acc ls = Token   (reverse acc) : ls
    finish = Right . reverse
    fix ls acc s | null s = finish $ emit acc ls
    fix ls acc s | isSpace (head s) = fix (emit acc ls) empty (tail s)
    fix ls acc s | '\"' == head s =
      let l' = emit acc ls in
      case reads s of
        [(str, rest)] -> fix (String str : l') empty rest
        _ -> Left $ "error parsing string: " ++ s
    fix ls acc s | '#' == head s =
      finish . (Comment (tail s) :) . (emit acc) $ ls
    fix ls acc s = fix ls (head s : acc) (tail s)

-- TODO support multiline strings?
lexFile :: String -> Either Error [[L]]
lexFile = mapM step . zip [1..] . lines
  where
    step (i, l) =
      case spaceout <$> lexLine l of
        Left err -> Left ("line " ++ show i ++": " ++ err)
        Right v -> Right v

int_ :: LParser Int
int_ = ParseEither step
  where
    step ts0@(Token s : ts) =
      case reads s of
        (i, "") : _ -> Right (i, ts)
        _ -> Left (msg, ts0)
    step s = Left (msg, s)
    msg = "expected integer"

char :: Char -> LParser Char
char c = ParseEither (char' c)
  where
    char' :: Char -> [L] -> Either (Error, [L]) (Char, [L])
    char' c (Token (c' : cs) : ts) | c == c' = Right (c, Token cs : ts)
    char' c s@(Token [] : ts) = err s
    char' c s = err s
    err s = Left ("expected char: " ++ [c], s)

token :: LParser String
token = ParseEither ok
  where
    ok ts@(Token t : _) | t `elem` map (:[]) forbidden_characters = Left ("invalid token: " ++ t, ts)
    ok (Token t : ts) = Right (t, ts)
    ok ts@(t:_) = Left $ ("expected token, got: " ++ show t, ts)
    ok [] = Left ("expected token, got EOF.", [])

identifier :: LParser String
identifier = ParseEither ok
  where
    ok ts@(Token t : _) | t `elem` map (:[]) forbidden_characters = Left ("invalid token: " ++ t, ts)
    ok ts@(Token t : _) | t `elem` forbidden_identifier = Left ("invalid token: " ++ t, ts)
    ok ts@(Token t : _) | head t == '\'' = Left ("invalid token: " ++ t, ts)
    ok (Token t : ts) = Right (t, ts)
    ok ts@(t:_) = Left $ ("expected token, got: " ++ show t, ts)
    ok [] = Left ("expected token, got EOF.", [])

stringLiteral :: LParser String
stringLiteral = ParseEither ok
  where
    ok (String s : ts) = Right (s, ts)
    ok ts = Left ("expected string literal", ts)

string :: String -> LParser String
string str = ParseEither go
  where
    go (Token s : ts) | s == str = Right (s, ts)
    go s = Left ("expected token: " ++ str, s)

comma_ = string ","
symbol_ = char '\'' *> token
hole_ = string "_"
q_ = (NVal <$>
      ((NSymbol <$> symbol_)
        <|> (NInt <$> int_)
        <|> (NString <$> stringLiteral)))
     <|> (pure NHole <* hole_)
     <|> (NVar <$> identifier)
rel_ = L <$> identifier
ineq_ =
  (string "=" *> return QEq) <|>
  (string "/=" *> return QDisEq) <|>
  (string "<=" *> return QLessEq) <|>
  (string ">=" *> return QMoreEq) <|>
  (string "<" *> return QLess) <|>
  (string ">" *> return QMore)
qbin_ = flip QBinOp <$> expr_ <*> ineq_ <*> expr_
ep_ lin = EP lin <$> rel_ <*> many q_
lp_ p = LP p <$> rel_ <*> many q_

query_ = Query Low <$> ep_ NonLinear
dotquery_ = string "." *> (Query High <$> ep_ NonLinear)
linearquery_ = string "." *> string "." *> (Query Low <$> ep_ Linear)
dotlinearquery_ = string "." *> string "." *> string "." *> (Query High <$> ep_ Linear)
lnlogicquery  = string "!" *> (Query Low <$> lp_ Negative)
hnlogicquery  = string ".!" *> (Query High <$> lp_ Negative)

clause_ = qbin_ <|> dotquery_ <|> linearquery_ <|> dotlinearquery_ <|> query_
        <|> lnlogicquery <|> hnlogicquery
lhs_ = sepBy1 comma_ clause_

wrap_ p = (string "(") *> p <* (string ")")

binOp_ =
  ((string "+") *> return Sum)
  <|> ((string "*") *> return Mul)
  <|> ((string "-") *> return Sub)

expr_ =
  (ELit <$> int_)
  <|> (ENamed <$> symbol_)
  <|> (pure EHole <* hole_)
  <|> (EVar <$> identifier)
  <|> (EString <$> stringLiteral)
  <|> (wrap_ $ flip EBinOp <$> expr_ <*> binOp_ <*> expr_)
  <|> (wrap_ $ EConcat <$> expr_ <*> (string "++" *> expr_))

-- allow trailing whitespace?
rquery_ = Assert <$> rel_ <*> many expr_
rclause_ = rquery_
rhs_ = sepBy comma_ rclause_

arrow_ = string "=>"
larrow_ = string "~>"

rule_ = (Rule Event <$> lhs_ <*> (arrow_ *> rhs_))
lrule_ = (Rule View <$> lhs_ <*> (larrow_ *> rhs_))

line_ = rule_ <|> lrule_

parseLine line s =
  case runParser line_ s of
    Right (r, []) -> (line, r)
    p -> error $ "line " ++ show line ++ ": incomplete parse.\n  " ++ show p

removeComments = filter (not . isComment)

parseTuple :: [L] -> Either Error Assert
parseTuple ls =
  case runParser rquery_ ls of
    Left (err, _) -> Left err
    Right (v, []) -> Right v
    Right v -> Left $ "incomplete parse: " ++ show v

parseTupleFile :: String -> Either Error [[Assert]]
parseTupleFile f = do
    ls <- lexFile f
    let bs = filter (not . null) . map fixBlock . blocks $ ls
    mapM (mapM parseTuple) bs
  where
    blocks :: [[L]] -> [[[L]]]
    blocks [] = []
    blocks xs = let (x, r) = span (not . null) xs in x : blocks (st r)
      where
        st [] = []
        st (_:a) = a
    fixBlock :: [[L]] -> [[L]]
    fixBlock = nonEmpty . map removeComments
    nonEmpty = filter (not . null)

parseRuleFile :: String -> Either Error [LineRule]
parseRuleFile x = do
  ls <- lexFile x
  let numbered = zip [1..] ls
  let nonEmptyLines = filter (not . null . snd) . map (second removeComments) $ numbered
  return $ map (uncurry parseLine) nonEmptyLines
