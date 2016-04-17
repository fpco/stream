-- FIXME move to stream-core library
{-# LANGUAGE RankNTypes #-}
module Stream.Core
    ( -- * Types
      StreamT
    , Step (..)
      -- * Helpers
    , mapStep
    , runStreamT
      -- * Smart constructros
      -- ** Source
    , makeSource
    , makeSourceWith
      -- ** Transformer
    , makeTransformer
    , makeTransformer'
      -- ** Sink
    , makeSink
    ) where

import Stream.Core.Internal

mapStep :: (i -> o) -> Step s i r -> Step s o r
mapStep _ (Done r) = Done r
mapStep f (Yield s i) = Yield s (f i)
mapStep _ (Skip s) = Skip s
{-# INLINE mapStep #-}

runStreamT :: Monad m => StreamT o m r -> m r
runStreamT =
    makeSink go
  where
    go s0 f =
        loop s0
      where
        loop s = do
            step <- f s
            case step of
                Done r -> pure r
                Yield s' _ -> loop s'
                Skip s' -> loop s'
{-# INLINE runStreamT #-}

makeSource :: state
           -> (state -> m (Step state o r))
           -> StreamT o m r
makeSource state f = StreamT f ($ state)
{-# INLINE makeSource #-}

makeSourceWith :: (forall b. (state -> m b) -> m b)
               -> (state -> m (Step state o r))
               -> StreamT o m r
makeSourceWith withState f = StreamT f withState
{-# INLINE makeSourceWith #-}

makeTransformer :: myState
                -> (forall upState.
                       myState
                    -> upState
                    -> (upState -> m (Step upState i upR))
                    -> m (Step (myState, upState) o myR))
                -> StreamT i m upR
                -> StreamT o m myR
makeTransformer myState f (StreamT g withUpState) =
    StreamT (\(myState, upState) -> f myState upState g) withState
  where
    withState inner = withUpState $ \upState -> inner (myState, upState)

makeTransformer' :: (forall upState.
                        upState
                     -> (upState -> m (Step upState i upR))
                     -> m (Step upState o myR))
                 -> StreamT i m upR
                 -> StreamT o m myR
makeTransformer' f (StreamT g withState) =
    StreamT (\s -> f s g) withState
{-# INLINE makeTransformer' #-}

makeSink :: (forall state. state -> (state -> m (Step state i upR)) -> m myR)
         -> StreamT i m upR
         -> m myR
makeSink f (StreamT g withState) = withState $ \state -> f state g
{-# INLINE makeSink #-}
