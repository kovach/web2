module Queue
  (Q, empty, snoc, tail, head, null)
  where
import Prelude hiding (tail, head, null)
-- Real time (O(1) worst case) single-ended queue, from Okasaki 7.2
-- A queue has a front, a back, and a "schedule"
data Q a = Q [a] [a] [a]
  deriving Show
empty = Q [] [] []

-- incremental rotate/append
rotateApp [] [y] acc = y : acc
rotateApp (x:xs) (y:ys) acc = x : rotateApp xs ys (y:acc)
exec :: Q a -> Q a
exec (Q f r (_:s)) = Q f r s
-- when s (the "schedule") is empty, it must be the case that |f|+1 = |r|, which
-- means it's time to rotate
exec (Q f r []) =
  let f' = rotateApp f r []
  -- front = schedule
  in Q f' [] f'

-- snoc (insert at end) and tail take guaranteed constant time:
--   each time we use either, exec processes the next element of s, which
--   incrementally evaluates the append/reverse operation above
snoc :: a -> Q a -> Q a
snoc x (Q f r s) = exec $ Q f (x : r) s

tail :: Q a -> Q a
tail (Q (x:f) r s) = exec $ Q f r s

head :: Q a -> a
head (Q (x:_) _ _) = x

null :: Q a -> Bool
null (Q [] [] []) = True
null _ = False
