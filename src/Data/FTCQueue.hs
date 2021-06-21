{-# LANGUAGE GADTs #-}
module Data.FTCQueue where

-- pop from here
data Kleisli m a b where
    Singleton :: (a -> m b) -> Kleisli m a b
    ComposeL :: (a -> m b) -> Kleisli m b c -> Kleisli m a c

-- push to here
data KleisliReverse m a b where
    SingletonReverse :: (a -> m b) -> KleisliReverse m a b
    ComposeR :: KleisliReverse m a b -> (b -> m c) -> KleisliReverse m a c

data KleisliQueue m a b
    = KleisliQueue !Int (Kleisli m a b) !Int (KleisliReverse m a b)

check :: KleisliQueue m a b -> KleisliQueue m a b
check (KleisliQueue lenf f lenr r) = undefined
