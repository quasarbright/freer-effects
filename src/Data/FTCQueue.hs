{-# LANGUAGE GADTs #-}
module Data.FTCQueue where

-- pop from here
data Kleisli m a b where
    Singleton :: (a -> m b) -> Kleisli m a b
    Compose :: (a -> m b) -> Kleisli m b c -> Kleisli m a c

-- push to here
data KleisliReverse m a b where
    SingletonReverse :: (a -> m b) -> KleisliReverse m a b
    ComposeReverse :: KleisliReverse m b c -> (a -> m b) -> KleisliReverse m a c

data KleisliQueue m a b
    = KleisliQueue !Int (KleisliReverse m a b) !Int (Kleisli m a b)

reverseKleisli :: KleisliReverse m a b -> Kleisli m a b
reverseKleisli (SingletonReverse k) = Singleton k
reverseKleisli (ComposeReverse ks k) = Compose k (reverseKleisli ks)

appendKleisli :: Kleisli m a b -> Kleisli m b c -> Kleisli m a c
appendKleisli (Singleton k) ks = Compose k ks
appendKleisli (Compose k ks) ks' = Compose k (appendKleisli ks ks')

-- can't just append and empty bc there is no empty kleisli. maybe allow empty reverse, but not empty forward? Only problem with empty is appending anyway
check :: KleisliQueue m a b -> KleisliQueue m a b
check (KleisliQueue lenh h lent t)
    | lenh > lent = _
