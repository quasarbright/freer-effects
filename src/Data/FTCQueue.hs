{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Data.FTCQueue
    ( FTCQueue()
    , tsingleton
    , (|>)
    , postCompose
    , (<|)
    , preCompose
    , (|><|)
    , ViewL(..)
    , tviewl
    ) where

import Prelude hiding (reverse, id, (.))
import Control.Category ( Category(..) )
import Control.Arrow ( Arrow((***), first, arr) )
import Data.Tuple (swap)

data FTCQueue m a b where
    Leaf :: (a -> m b) -> FTCQueue m a b
    Node :: FTCQueue m a b -> FTCQueue m b c -> FTCQueue m a c

instance Monad m => Category (FTCQueue m) where
    id = tsingleton return
    (.) = flip append

instance Monad m => Arrow (FTCQueue m) where
    arr f = tsingleton (return . f)
    first (Leaf k) = Leaf (\(a,b) -> (,b) <$> k a)
    first (Node l r) = Node (first l) (first r)
    f *** g = first f |><| arr swap |><| first g |><| arr swap


---------------
-- interface --
---------------


-- | lift a single function into the queue
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Leaf

-- | Add a function to the queue (post-composition)
infixl |>
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
l |> k = Node l (Leaf k)

-- | Alias for `(|>)`
postCompose :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
postCompose = (|>)

infixr <|
(<|) :: (a -> m b) -> FTCQueue m b c -> FTCQueue m a c
k <| r = Node (Leaf k) r

preCompose :: (a -> m b) -> FTCQueue m b c -> FTCQueue m a c
preCompose = (<|)

-- | Compose/append two queues
infixr |><|
(|><|) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(|><|) = Node

-- | Alias for `(|><|)`
append :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
append = (|><|)

-- | Data structure representing the result of a "pop" from the queue
data ViewL m a b where
    TOne :: (a -> m b) -> ViewL m a b
    (:|) :: (a -> m x) -> FTCQueue m x b -> ViewL m a b

-- | "Pop" the function which needs to be executed first
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf k) = TOne k
tviewl (Node l r) = popl l r

-- | Use tree rotations to access the left. Also rotates the right tree in the process, making the next access more efficient
-- as long as it uses the result of the ViewL. Apparently O(1) average time
popl :: FTCQueue m a b -> FTCQueue m b c -> ViewL m a c
popl (Leaf k) r = k :| r
popl (Node a b) c = popl a (Node b c)
