module Queue ( Queue, empty, fromList, Queue.concat, pop, push ) where

data Queue a = Cons [a] [a] | Conc (Queue a) (Queue a) deriving Show

empty :: Queue a
empty = Cons [] []


-- | (in-order : head is popped before tail)
fromList :: [a] -> Queue a
fromList = Cons []

concat :: Queue a -> Queue a -> Queue a
concat = Conc

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Cons a b)
  | null a && null b = (Nothing, q)
  | null b           = let reva = reverse a in (Just $ head reva, Cons [] $ tail reva)
  | otherwise        = (Just $  head b, Cons a $ tail b)
pop (Conc a b) = let popa = pop a in case fst popa of
  Nothing   -> pop b
  Just elem -> (fst popa, Conc (snd popa) b)

push :: a -> Queue a -> Queue a
push elem (Cons a b) = Cons (elem : a) b
push elem (Conc a b) = Conc a $ push elem b
