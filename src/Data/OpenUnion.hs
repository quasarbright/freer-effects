{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.OpenUnion where

import Data.Kind (Constraint)
data Union (ts :: [* -> *]) a where
    UNow :: t a -> Union (t ': ts) a
    UNext :: Union ts a -> Union (any ': ts) a

data Nat = Z | S Nat

data P (n :: Nat) = P

class Member' (t :: * -> *) (ts :: [* -> *]) (n :: Nat) where
    inj' :: P n -> t a -> Union ts a
    prj' :: P n -> Union ts a -> Maybe (t a)

instance (ts ~ (t ': ts')) => Member' t ts 'Z where
    inj' _ = UNow
    prj' _ (UNow t) = Just t
    prj' _ (UNext _) = Nothing

instance (ts ~ (any ': t' ': ts'), Member' t (t' ': ts') n) => Member' t ts ('S n) where
    inj' _ = UNext . inj' (P :: P n)
    prj' _ (UNow _) = Nothing
    prj' _ (UNext u) = prj' (P :: P n) u

type family Find (t :: * -> *) (ts :: [* -> *]) :: Nat where
    Find t (t ': ts) = 'Z
    Find t (any ': t' ': ts) = Find t (t' ': ts)

class Member (t :: * -> *) (ts :: [* -> *]) where
    inj :: t a -> Union ts a
    prj :: Union ts a -> Maybe (t a)

instance (Member' t ts (Find t ts)) => Member t ts where
    inj = inj' (P :: P (Find t ts))
    prj = prj' (P :: P (Find t ts))

instance Functor (Union '[]) where
    fmap _ = done

instance (Functor t, Functors ts) => Functor (Union (t ': ts)) where
    fmap f (UNow t) = UNow (fmap f t)
    fmap f (UNext u) = UNext (fmap f u)
    

instance Foldable (Union '[]) where
    foldMap _ = done

instance (Foldable t, Foldable (Union ts)) => Foldable (Union (t ': ts)) where
    foldMap f (UNow t) = foldMap f t
    foldMap f (UNext u) = foldMap f u

instance Traversable (Union '[]) where
    traverse _ = done

instance (Traversable t, Traversable (Union ts)) => Traversable (Union (t ': ts)) where
    traverse f (UNow t) = UNow <$> traverse f t
    traverse f (UNext u) = UNext <$> traverse f u

weaken :: Union ts a -> Union (t ': ts) a
weaken = UNext

decompose :: Union (t ': ts) a -> Either (Union ts a) (t a)
decompose (UNow t) = Right t
decompose (UNext u) = Left u

extract :: Union '[t] a -> t a
extract (UNow t) = t
extract (UNext u) = done u

done :: Union '[] a -> b
done u = case u of

type Functors (ts :: [* -> *]) = Functor (Union ts)

type family Members (ts :: [* -> *]) (r :: [* -> *]) :: Constraint where
    Members '[t] r = Member t r
    Members (t ': ts) r = (Member t r, Members ts r)

-- | Polymorphic raising
class Raise (r :: [* -> *]) (r' :: [* -> *]) where
    weaken' :: Union r x -> Union r' x

instance Raise r r where
    weaken' = id

instance (Raise r r') => Raise r (f ': r') where
    weaken' = weaken . weaken'
