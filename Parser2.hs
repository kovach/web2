module Parser2 where
import Types
import Parse

comma_ = token $ char ','
symbol_ = char '\'' *> identifier
q_ = (QVal <$> ((NTNamed <$> symbol_) <|> (NTInt <$> int_))) <|> (QVar <$> identifier)
rel_ = token $ L <$> identifier
dotrel_ = char '.' *> rel_
hashrel_ = char '#' *> rel_
ineq_ =
  (token (string "=") *> return QEq) <|>
  (token (string "/=") *> return QDisEq) <|>
  (token (string "<") *> return QLess) <|>
  (token (string ">") *> return QMore)
qbin_ = token $ flip QBinOp <$> token q_ <*> ineq_ <*> token q_
ep_ = token $
  ((EP <$> rel_ <*> sepBy flex1 q_)
   ) -- <|> (char '@' *> (Tuple <$> rel_ <*> sepBy flex1 q_)))
--pred_ = token $ (Query <$> (EP <$> rel_ <*> pure (QVal rootNode) <*> q_))
query_ = token $ Query <$> ep_
dotquery_ = token $ char '.' *> (DotQuery <$> ep_)
hashquery_ = token $ char '#' *> (HashQuery <$> ep_)
countquery_ = token . bracket_ $ Counter <$> names_ <* sep <*> lhs_
  where
    sep = token (char '|')
    names_ = token $ sepBy1 flex1 identifier

clause_ = qbin_ <|> dotquery_ <|> hashquery_ <|> query_ <|> countquery_
lhs_ = sepBy1 comma_ clause_

arrow_ = token $ string "=>"
wrap_ p = (token $ char '(') *> token p <* (token $ char ')')
bracket_ p = (token $ char '[') *> token p <* (token $ char ']')

binOp_ =
  (token (char '+') *> return Sum)
  <|> (token (char '*') *> return Mul)
  <|> (token (char '-') *> return Sub)
expr_ =
  (ELit <$> int_)
  <|> (EVar <$> identifier)
  <|> (ENamed <$> symbol_)
  <|> (wrap_ $ flip EBinOp <$> token expr_ <*> binOp_ <*> token expr_)

--rpred_ = flip Assert rootExpr <$> rel_ <*> expr_
rquery_ = Assert <$> rel_ <*> sepBy flex1 expr_
rclause_ = rquery_
rhs_ = sepBy1 comma_ rclause_


rule_ = Rule <$> (token lhs_) <*> (arrow_ *> rhs_)

parse s =
  case runParser rule_ s of
    Right (r, "") -> r
    p -> error $ "bad parse: " ++ show p


notComment ('#' : _) = False
notComment "" = False
notComment _ = True

readRules f = do
  rs <- filter notComment . lines <$> readFile f
  return $ map parse rs
