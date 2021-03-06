{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Eff
    ( Eff()
    , liftEff
    , interpret
    , interpret'
    , reinterpret
    , rewrite
    , run
    , raise
    , raise'
    , Member()
    , Members()
    ) where

import Prelude hiding (id, (.))
import Data.OpenUnion
    ( weaken, decompose, Raise(..), Member(inj), Members(), done )
import Data.FTCQueue
    ( ViewL((:|), TOne), tsingleton, (|><|), tviewl )
import Control.Category ( Category((.), id) )
import Control.Monad.Eff.Internal ( Eff(..), Arrs )

-- | Apply a kleisli queue as a function
qApp :: Arrs r a b -> a -> Eff r b
qApp q a = case tviewl q of
    TOne f -> f a
    f :| q' -> bind' (f a) q'

-- | Bind a kleisli queue as if it was a single kleisli
bind' :: Eff r a -> Arrs r a b -> Eff r b
bind' (Pure a) k = qApp k a
bind' (Bind m k) k' = Bind m (k |><| k')

-- | Composes a kleisli queue with a polymorphic Eff to Eff function which can change row type.
-- The result is a kleisli queue in the new effect row, with the function post-composed
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComp q f = tsingleton (f . qApp q)

-- | Lifts a raw effect into the monad
liftEff :: Member f r => f x -> Eff r x
liftEff f = Bind (inj f) id

-- | Tool for writing simple interpreters with a handler that extracts an effect's value
interpret :: (forall x . f x -> x) -> Eff (f ': r) a -> Eff r a
interpret handler = interpret' pure (\fx k -> k (handler fx))

-- | Tool for writing more general interpreters. Must supply an analog for return and bind. Also functions as a polymorphic `reinterpret` if you constrain @r@
interpret' :: (a -> Eff r b) -> (forall x . f x -> (x -> Eff r b) -> Eff r b) -> Eff (f ': r) a -> Eff r b
interpret' ret _ (Pure a) = ret a
interpret' ret bind (Bind u k) = case decompose u of
    Left u' -> Bind u' (k `qComp` interpret' ret bind)
    Right f -> bind f (interpret' ret bind . qApp k)

-- | Tool for writing interpreters which convert the top effect type into another effect type deeper in the stack
reinterpret :: Member f' r => (forall x . f x -> f' x) -> Eff (f ': r) a -> Eff r a
reinterpret _ (Pure a) = Pure a
reinterpret f (Bind u k) = case decompose u of
    Left u' -> Bind u' (k `qComp` reinterpret f)
    Right fx -> Bind (inj (f fx)) (k `qComp` reinterpret f)

-- | Tool for writing interpreters which converts the top effect type to another effect type on the top of the stack
rewrite :: forall f f' r a . (forall x . f x -> f' x) -> Eff (f ': r) a -> Eff (f' ': r) a
rewrite _ (Pure a) = Pure a
rewrite f (Bind u k) = case decompose u of
    Left u' -> Bind (weaken u') (k `qComp` rewrite f)
    Right fx -> Bind (inj (f fx)) (k `qComp` rewrite f)

run :: Eff '[] a -> a
run (Pure a) = a
run (Bind u _) = done u

-- | Add a possible effect to the row of effects
raise :: Eff r a -> Eff (f ': r) a
raise (Pure a) = Pure a
raise (Bind u k) = Bind (weaken u) (k `qComp` raise)

-- | Add an arbitrary number of effects to the row of effects. Recommend using `raise` when possible for better type errors and inference
raise' :: Raise r r' => Eff r a -> Eff r' a
raise' (Pure a) = Pure a
raise' (Bind u k) = Bind (weaken' u) (k `qComp` raise')
