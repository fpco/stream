-- FIXME move to stream-core library
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Stream.Core.Internal where

data Step s o r
    = Done r
    | Yield s o
    | Skip s

data StreamT o m r = forall s. StreamT
    (s -> m (Step s o r))
    (forall b. (s -> m b) -> m b)
