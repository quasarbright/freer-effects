{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Eff where

import Data.OpenUnion
import Control.Monad ((>=>))

data Eff r a where
    Pure :: a -> Eff r a
    Bind :: Union r x -> (x -> Eff r a) -> Eff r a

instance Functor (Eff r) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Bind m k) = Bind m (fmap f . k)

instance Applicative (Eff r) where
    pure = Pure
    Pure a <*> eff = a <$> eff
    Bind m k <*> eff = Bind m ((<*> eff) . k)

instance Monad (Eff r) where
    return = pure
    Pure a >>= k = k a
    Bind m k >>= k' = Bind m (k >=> k')

liftEff :: Member f r => f x -> Eff r x
liftEff f = Bind (inj f) return

interpret :: (forall x . f x -> x) -> Eff (f ': r) a -> Eff r a
interpret _ (Pure a) = Pure a
interpret f (Bind u k) = case decompose u of
    Left u' -> Bind u' (interpret f . k)
    Right fx -> interpret f (k (f fx))

reinterpret :: Member f' r => (forall x . f x -> f' x) -> Eff (f ': r) a -> Eff r a
reinterpret _ (Pure a) = Pure a
reinterpret f (Bind u k) = case decompose u of
    Left u' -> Bind u' (reinterpret f . k)
    Right fx -> Bind (inj (f fx)) (reinterpret f . k)

rewrite :: forall f f' r a . (forall x . f x -> f' x) -> Eff (f ': r) a -> Eff (f' ': r) a
rewrite _ (Pure a) = Pure a
rewrite f (Bind u k) = case decompose u of
    Left u' -> Bind (weaken u') (rewrite f . k)
    Right fx -> Bind (inj (f fx)) (rewrite f . k)

-- | Add a possible effect to the row of effects
raise :: Eff r a -> Eff (f ': r) a
raise (Pure a) = Pure a
raise (Bind u k) = Bind (weaken u) (raise . k)

-- | Add an arbitrary number of effects to the row of effects. Recommend using `raise` when possible for better type errors and inference
raise' :: Raise r r' => Eff r a -> Eff r' a
raise' (Pure a) = Pure a
raise' (Bind u k) = Bind (weaken' u) (raise' . k)
