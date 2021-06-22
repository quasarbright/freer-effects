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

import Prelude hiding (reverse)
import Control.Category ( Category(..) )

-- | Queue head. Push onto here for post-composition
data Head m a b where
    Empty :: Head m a a
    Push :: (b -> m c) -> Head m a b -> Head m a c

-- | Queue tail. Pop from here to extract the function that must be executed first. Cons for pre-composition
data Tail m a b where
    Singleton :: (a -> m b) -> Tail m a b
    Cons :: (a -> m b) -> Tail m b c -> Tail m a c

-- | Convert head to tail in `O(n)` time. Uses a strict left fold, so shouldn't have a space leak
headToTail :: Head m a b -> Tail m a b
headToTail (Push k h) = headToTailHelp h (Singleton k)
headToTail Empty = error "cannot convert empty head to tail"

-- | Helper for `headToTail` for accumulating. Made it a standalone because there was inference trouble
headToTailHelp :: Head m a b -> Tail m b c -> Tail m a c
headToTailHelp Empty acc = acc
headToTailHelp (Push k h) acc = headToTailHelp h $! Cons k acc

-- | Append/compose two tails. `O(n)` time where `n` is the length of the left tail
appendTail :: Tail m a b -> Tail m b c -> Tail m a c
appendTail (Singleton k) t = Cons k t
appendTail (Cons k t) t' = Cons k (appendTail t t')

data FTCQueue m a b where
    FTCQueue :: !Int -> Head m b c -> !Int -> Tail m a b -> FTCQueue m a c

-- | If the head is larger than the tail, shift the whole head onto the tail. Ensures amortized constant time access
check :: FTCQueue m a b -> FTCQueue m a b
check q@(FTCQueue lenh h lent t)
    | lenh > lent = FTCQueue 0 Empty (lenh + lent) (appendTail t (headToTail h))
    | otherwise = q

instance Monad m => Category (FTCQueue m) where
    id = tsingleton return
    (.) = flip append

---------------
-- interface --
---------------

-- | lift a single function into the queue
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton k = FTCQueue 0 Empty 1 (Singleton k)

-- | Add a function to the queue (post-composition)
infixr |>
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
FTCQueue lenh h lent t |> k = check $ FTCQueue (succ lenh) (Push k h) lent t

-- | Alias for `(|>)`
postCompose :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
postCompose = (|>)

-- | Compose/append two queues
infixr |><|
(|><|) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
FTCQueue lenh h lent t |><| FTCQueue lenh' h' lent' t' = FTCQueue lenh h' (lent + lenh' + lent') (t `appendTail` headToTail h `appendTail` t')

-- | Alias for `(|><|)`
append :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
append = (|><|)

-- | Data structure representing the result of a "pop" from the queue
data ViewL m a b where
    TOne :: (a -> m b) -> ViewL m a b
    (:|) :: (a -> m x) -> FTCQueue m x b -> ViewL m a b

-- | "Pop" the function which needs to be executed first
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (FTCQueue _ Empty _ (Singleton k)) = TOne k
tviewl (FTCQueue _ Push{} _ Singleton{}) = error "called tviewl on a malformed queue"
tviewl (FTCQueue lenh h lent (Cons k t)) = k :| FTCQueue lenh h (pred lent) t
