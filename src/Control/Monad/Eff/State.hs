{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Eff.State where

import Control.Monad.Eff
data State s a where
    Get :: State s s
    Put :: s -> State s ()

get :: Member (State s) r => Eff r s
get = liftEff Get

put s = liftEff (Put s)
