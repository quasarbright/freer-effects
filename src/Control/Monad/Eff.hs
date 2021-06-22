{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Eff where

import Prelude hiding (id, (.))
import Data.OpenUnion
    ( weaken, decompose, Raise(..), Member(inj), Union )
import Data.FTCQueue
    ( (|>), FTCQueue, ViewL((:|), TOne), tsingleton, (|><|), tviewl )
import Control.Category ( Category((.), id) )

type Arr r a b = a -> Eff r b
type Arrs r a b = FTCQueue (Eff r) a b

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

qApp :: Arrs r a b -> a -> Eff r b
qApp q a = case tviewl q of
    TOne f -> f a
    f :| q' -> bind' (f a) q'

bind' :: Eff r a -> Arrs r a b -> Eff r b
bind' (Pure a) k = qApp k a
bind' (Bind m k) k' = Bind m (k |><| k')

-- | Composes a kleisli queue with a polymorphic Eff to Eff function which can change row type.
-- The result is a kleisli queue in the new effect row, with the function post-composed
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComp q f = tsingleton (f . qApp q)

liftEff :: Member f r => f x -> Eff r x
liftEff f = Bind (inj f) id

interpret :: (forall x . f x -> x) -> Eff (f ': r) a -> Eff r a
interpret _ (Pure a) = Pure a
interpret f (Bind u k) = case decompose u of
    Left u' -> Bind u' (k `qComp` interpret f)
    Right fx -> interpret f (k `qApp` f fx)

interpret' :: (a -> Eff r b) -> (forall x . f x -> (x -> Eff r b) -> Eff r b) -> Eff (f ': r) a -> Eff r b
interpret' ret _ (Pure a) = ret a
interpret' ret bind (Bind u k) = case decompose u of
    Left u' -> Bind u' (k `qComp` interpret' ret bind)
    Right f -> bind f (interpret' ret bind . qApp k)

reinterpret :: Member f' r => (forall x . f x -> f' x) -> Eff (f ': r) a -> Eff r a
reinterpret _ (Pure a) = Pure a
reinterpret f (Bind u k) = case decompose u of
    Left u' -> Bind u' (k `qComp` reinterpret f)
    Right fx -> Bind (inj (f fx)) (k `qComp` reinterpret f)

rewrite :: forall f f' r a . (forall x . f x -> f' x) -> Eff (f ': r) a -> Eff (f' ': r) a
rewrite _ (Pure a) = Pure a
rewrite f (Bind u k) = case decompose u of
    Left u' -> Bind (weaken u') (k `qComp` rewrite f)
    Right fx -> Bind (inj (f fx)) (k `qComp` rewrite f)

-- | Add a possible effect to the row of effects
raise :: Eff r a -> Eff (f ': r) a
raise (Pure a) = Pure a
raise (Bind u k) = Bind (weaken u) (k `qComp` raise)

-- | Add an arbitrary number of effects to the row of effects. Recommend using `raise` when possible for better type errors and inference
raise' :: Raise r r' => Eff r a -> Eff r' a
raise' (Pure a) = Pure a
raise' (Bind u k) = Bind (weaken' u) (k `qComp` raise')
