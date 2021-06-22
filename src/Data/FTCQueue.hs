{-# LANGUAGE GADTs #-}
module Data.FTCQueue
    ( FTCQueue()
    , tsingleton
    , (|>)
    , postCompose
    , (|><|)
    , ViewL(..)
    , tviewl
    ) where

import Prelude hiding (reverse, id, (.))
import Control.Category ( Category(..) )
import Control.Arrow
import Data.Tuple (swap)

-- | Queue head. Push onto here for post-composition
data Head arr a b where
    Empty :: Head arr a a
    Push :: arr b c -> Head arr a b -> Head arr a c

-- | Queue tail. Pop from here to extract the function that must be executed first. Cons for pre-composition
data Tail arr a b where
    Singleton :: arr a b -> Tail arr a b
    Cons :: arr a b -> Tail arr b c -> Tail arr a c

-- | Convert head to tail in `O(n)` time. Uses a strict left fold, so shouldn't have a space leak
headToTail :: Head arr a b -> Tail arr a b
headToTail (Push k h) = headToTailHelp h (Singleton k)
headToTail Empty = error "cannot convert empty head to tail"

-- | Helper for `headToTail` for accumulating. Made it a standalone because there was inference trouble
headToTailHelp :: Head arr a b -> Tail arr b c -> Tail arr a c
headToTailHelp Empty acc = acc
headToTailHelp (Push k h) acc = headToTailHelp h $! Cons k acc

-- | Append/compose two tails. `O(n)` time where `n` is the length of the left tail
infixr `appendTail`
appendTail :: Tail arr a b -> Tail arr b c -> Tail arr a c
appendTail (Singleton k) t = Cons k t
appendTail (Cons k t) t' = Cons k (appendTail t t')

data FTCQueue arr a b where
    FTCQueue :: !Int -> Head arr b c -> !Int -> Tail arr a b -> FTCQueue arr a c

-- | If the head is larger than the tail, shift the whole head onto the tail. Ensures amortized constant time access
check :: FTCQueue arr a b -> FTCQueue arr a b
check q@(FTCQueue lenh h lent t)
    | lenh > lent = FTCQueue 0 Empty (lenh + lent) (appendTail t (headToTail h))
    | otherwise = q

instance Category arr => Category (Head arr) where
    id = Push id Empty
    Empty . h = h
    Push k h . h' = Push k (h . h')

instance Arrow arr => Arrow (Head arr) where
    arr f = Push (arr f) Empty
    first Empty = Empty
    first (Push k h) = Push (first k) (first h)

instance Category arr => Category (Tail arr) where
    id = Singleton id
    (.) = flip appendTail

instance Arrow arr => Arrow (Tail arr) where
    arr = Singleton . arr
    first (Singleton k) = Singleton (first k)
    first (Cons k t) = Cons (first k) (first t)

instance Category arr => Category (FTCQueue arr) where
    id = tsingleton id
    (.) = flip append

instance Arrow arr => Arrow (FTCQueue arr) where
    arr f = tsingleton (arr f)
    first (FTCQueue lenh h lent t) = FTCQueue lenh (first h) lent (first t)
    f *** g = first f |><| arr swap |><| first g |><| arr swap

-- instance Arrow arr => Arrow (FTCQueue arr) where
--     arr = tsingleton . arr
--     first (FTCQueue lenh h lent t) = case (h, t) of
--         (Empty, Cons k t') -> FTCQueue lenh Empty

---------------
-- interface --
---------------

-- | lift a single function into the queue
tsingleton :: arr a b -> FTCQueue arr a b
tsingleton k = FTCQueue 0 Empty 1 (Singleton k)

-- | Add a function to the queue (post-composition)
infixr |>
(|>) :: FTCQueue arr a x -> arr x b -> FTCQueue arr a b
FTCQueue lenh h lent t |> k = check $ FTCQueue (succ lenh) (Push k h) lent t

-- | Alias for `(|>)`
postCompose :: FTCQueue arr a x -> arr x b -> FTCQueue arr a b
postCompose = (|>)

-- | Compose/append two queues
infixr |><|
(|><|) :: FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b
FTCQueue lenh h lent t |><| FTCQueue lenh' h' lent' t' = FTCQueue lenh h' (lent + lenh' + lent') (t `appendTail` headToTail h `appendTail` t')

-- | Alias for `(|><|)`
append :: FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b
append = (|><|)

-- | Data structure representing the result of a "pop" from the queue
data ViewL arr a b where
    TOne :: arr a b -> ViewL arr a b
    (:|) :: arr a x -> FTCQueue arr x b -> ViewL arr a b

-- | "Pop" the function which needs to be executed first
tviewl :: FTCQueue arr a b -> ViewL arr a b
tviewl (FTCQueue _ Empty _ (Singleton k)) = TOne k
tviewl (FTCQueue _ Push{} _ Singleton{}) = error "called tviewl on a malformed queue"
tviewl (FTCQueue lenh h lent (Cons k t)) = k :| FTCQueue lenh h (pred lent) t
