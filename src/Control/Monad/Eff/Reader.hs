{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.Eff.Reader where

import Control.Monad.Eff

data Reader r a where
    Ask :: Reader r r

ask :: Member (Reader e) r => Eff r e
ask = liftEff Ask

asks :: Member (Reader e) r => (e -> e') -> Eff r e'
asks f = f <$> ask

runReader :: e -> Eff (Reader e ': r) a -> Eff r a
runReader e = interpret (\Ask -> e)

reader :: Member (Reader e) r => (e -> a) -> Eff r a
reader = asks

-- data Stream a = a :> Stream a

-- runConsumer :: Stream e -> Eff (Reader e ': r) a -> Eff r a
-- runConsumer (e:>es) = interpret' pure (\Ask k -> k e)
