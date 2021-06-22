{-# LANGUAGE GADTs #-}
module Control.Monad.Eff.Internal where

import Prelude hiding (id, (.))
import Data.OpenUnion
    ( Union )
import Data.FTCQueue
    ( (|>), FTCQueue )

type Arrs r a b = FTCQueue (Eff r) a b

-- | An extensible effect monad parametrized by effects @r@ and return type @a@
data Eff r a where
    Pure :: a -> Eff r a
    Bind :: Union r x -> Arrs r x a -> Eff r a

instance Functor (Eff r) where
    fmap f (Pure a) = Pure (f a)
    fmap f ma = ma >>= \a -> pure (f a)

instance Applicative (Eff r) where
    pure = Pure
    Pure a <*> eff = a <$> eff
    ff <*> fa = do
        f <- ff
        f <$> fa

instance Monad (Eff r) where
    return = Pure
    Pure a >>= k = k a
    Bind m k >>= k' = Bind m (k |> k')
