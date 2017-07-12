-- TODO combined pretty-printing?
module Parse where

import Control.Arrow (first)
import Data.List (foldl')

data ParseEither i e a =
  ParseEither { runParser :: i -> Either (e, i) (a, i) }

type Error = String
type Parser = ParseEither String Error

instance Functor (ParseEither str e) where
  fmap f p = ParseEither $ (fmap $ first f) . runParser p

instance Applicative (ParseEither str e) where
  pure x = ParseEither (\s -> return (x, s))
  f <*> x = ParseEither $ \s0 -> do
    (f', s1) <- runParser f s0
    (x', s2) <- runParser x s1
    return $ (f' x', s2)

instance Monad (ParseEither str e) where
  return = pure
  m >>= f = ParseEither $ \s0 -> do
    (val, s1) <- runParser m s0
    runParser (f val) s1

-- full backtracking
(<|>) :: ParseEither i e a -> ParseEither i e a -> ParseEither i e a
a <|> b = ParseEither $ \s ->
  case runParser a s of
    Left _ -> runParser b s
    Right v -> return v

failure :: Error -> ParseEither i Error a
failure str = ParseEither (\s -> Left (str, s))

assert :: Bool -> String -> ParseEither i Error ()
assert bool str =
  if bool then return () else failure str

many, many1 :: ParseEither i e a -> ParseEither i e [a]
many p = many1 p <|> return []
many1 p = (:) <$> p <*> many p
many_ p = return () <* many p
many1_ p = return () <* many1 p

sepBy, sepBy1 :: ParseEither i e s -> ParseEither i e a -> ParseEither i e [a]
sepBy1 sep p =
   (:) <$> p <*> ((sep *> sepBy1 sep p)  <|> return [])
sepBy sep p = sepBy1 sep p <|> return []

-- Typical String parsers
anyChar :: String -> Parser Char
anyChar str = foldr (<|>) (failure $ "expected one of: " ++ str) (map char str)

char :: Char -> Parser Char
char c = ParseEither (char' c)
  where
    char' :: Char -> String -> Either (Error, String) (Char, String)
    char' c (c' : cs) | c == c' = Right (c, cs)
    char' c s = Left ("expected char: " ++ [c], s)

string :: String -> Parser String
string = mapM char

alpha :: Parser Char
alpha = anyChar $ ['a'..'z'] ++ ['A'..'Z']

digit :: Parser Char
digit = anyChar $ ['0'..'9']

ws, flex, ws1, flex1 :: Parser ()
ws = many_ (anyChar " \t")
ws1 = many1_ (anyChar " \t")
flex = many_ (anyChar " \t\n")
flex1 = many1_ (anyChar " \t\n")

identifier :: Parser String
identifier = (:) <$> alpha <*> many (digit <|> alpha <|> anyChar "_")

int_ :: Parser Int
int_ = ParseEither step
  where
    step s = safeFirst s $ reads s
    msg = "expected integer" 
    safeFirst s [] = Left (msg, s)
    safeFirst _ (x : _) = Right x

token :: Parser a -> Parser a
token p = p <* flex
