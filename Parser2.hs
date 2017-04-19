module Parser2 where
import Types
import Parse

comma_ = token $ char ','
symbol_ = char '\'' *> identifier
q_ = token $ (QVal <$> ((NTNamed <$> symbol_) <|> (NTInt <$> int_))) <|> (QVar <$> identifier)
rel_ = token $ identifier
dotrel_ = char '.' *> rel_
hashrel_ = char '#' *> rel_
ineq_ =
  (token (string "=") *> return QEq)
  <|> (token (string "/=") *> return QDisEq)
  <|> (token (string "<") *> return QLess)
  <|> (token (string ">") *> return QMore)
qbin_ = token $ flip QBinOp <$> q_ <*> ineq_ <*> q_
pred_ = token $ (Query <$> (flip EP (QVal rootNode) <$> rel_ <*> q_))
query_ = token $ Query <$> (EP <$> rel_ <*> q_ <*> q_)
dotquery_ = token $ DotQuery <$> (EP <$> dotrel_ <*> q_ <*> q_)
hashquery_ = token $ HashQuery <$> (EP <$> hashrel_ <*> q_ <*> q_)
countquery_ = token . bracket_ $ Counter <$> names_ <* sep <*> lhs_
  where
    sep = token (char '|')
    names_ = token $ sepBy1 flex1 identifier

clause_ = query_ <|> dotquery_ <|> hashquery_ <|> pred_ <|> qbin_ <|> countquery_
lhs_ = sepBy1 comma_ clause_

arrow_ = token $ string "=>"
wrap_ p = (token $ char '(') *> token p <* (token $ char ')')
bracket_ p = (token $ char '[') *> token p <* (token $ char ']')

binOp_ =
  (token (char '+') *> return Sum)
  <|> (token (char '*') *> return Mul)
  <|> (token (char '-') *> return Sub)
expr_ = token $
  (ELit <$> int_)
  <|> (EVar <$> identifier)
  <|> (ENamed <$> symbol_)
  <|> (wrap_ $ flip EBinOp <$> expr_ <*> binOp_ <*> expr_)

rpred_ = flip Assert rootExpr <$> rel_ <*> expr_
rquery_ = Assert <$> rel_ <*> expr_ <*> expr_
rclause_ = rquery_ <|> rpred_
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
