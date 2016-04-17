{-# LANGUAGE BangPatterns #-}
module Stream
    ( -- * Types
      StreamT
      -- * Common
    , runStreamT

      -- * Sources
      -- ** Pure
    , enumFromToS
    , yieldS

      -- * Transformers
      -- ** Pure
    , mapS
      -- ** Monadic

      -- * Sinks
      -- ** Pure
    , foldlS
    , sumS
    , sinkListS
      -- ** Monadic
    , mapM_S

      -- * I/O
    , readFileS
    , writeFileS

      -- * Textual
    , linesAsciiS

      -- * Reexports
    , Identity (..)
    ) where

import Stream.Core
import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.Word
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as S
import qualified System.IO as IO
import qualified Data.Streaming.FileRead as FR
import qualified Data.Foldable as F

enumFromToS :: (Ord a, Enum a, Applicative m)
            => a
            -> a
            -> StreamT a m ()
enumFromToS start end =
    makeSource start go
  where
    go x
        | x <= end =
            let !y = succ x
             in pure (Yield y x)
        | otherwise = pure (Done ())
{-# INLINE [1] enumFromToS #-}

enumFromToS_num
    :: (Ord a, Num a, Applicative m)
    => a
    -> a
    -> StreamT a m ()
enumFromToS_num start end =
    makeSource start go
  where
    go x
        | x <= end =
            let !y = x + 1
             in pure (Yield y x)
        | otherwise = pure (Done ())
{-# INLINE [0] enumFromToS_num #-}
{-# RULES

"enumFromToS<Int>" enumFromToS =
    enumFromToS_num :: Applicative m => Int -> Int -> StreamT Int m ()

"enumFromToS<Int8>" enumFromToS =
    enumFromToS_num :: Applicative m => Int8 -> Int8 -> StreamT Int8 m ()

"enumFromToS<Int16>" enumFromToS =
    enumFromToS_num :: Applicative m => Int16 -> Int16 -> StreamT Int16 m ()

"enumFromToS<Int32>" enumFromToS =
    enumFromToS_num :: Applicative m => Int32 -> Int32 -> StreamT Int32 m ()

"enumFromToS<Int64>" enumFromToS =
    enumFromToS_num :: Applicative m => Int64 -> Int64 -> StreamT Int64 m ()

"enumFromToS<Word>" enumFromToS =
    enumFromToS_num :: Applicative m => Word -> Word -> StreamT Word m ()

"enumFromToS<Word8>" enumFromToS =
    enumFromToS_num :: Applicative m => Word8 -> Word8 -> StreamT Word8 m ()

"enumFromToS<Word16>" enumFromToS =
    enumFromToS_num :: Applicative m => Word16 -> Word16 -> StreamT Word16 m ()

"enumFromToS<Word32>" enumFromToS =
    enumFromToS_num :: Applicative m => Word32 -> Word32 -> StreamT Word32 m ()

"enumFromToS<Word64>" enumFromToS =
    enumFromToS_num :: Applicative m => Word64 -> Word64 -> StreamT Word64 m ()

"enumFromToS<Integer>" enumFromToS =
    enumFromToS_num :: Applicative m => Integer -> Integer -> StreamT Integer m () #-}

yieldS :: (F.Foldable f, Applicative m)
       => f o
       -> StreamT o m ()
yieldS x =
    makeSource (F.toList x) go
  where
    go [] = pure (Done ())
    go (y:ys) = pure (Yield ys y)
{-# INLINE yieldS #-}

mapS :: Applicative m
     => (a -> b)
     -> StreamT a m r
     -> StreamT b m r
mapS f =
    makeTransformer' go
  where
    go s g = fmap (mapStep f) (g s)
{-# INLINE mapS #-}

foldlS :: Monad m
       => (accum -> a -> accum)
       -> accum
       -> StreamT a m r
       -> m accum
foldlS f accum0 =
    makeSink go
  where
    go s0 g =
        loop accum0 s0
      where
        loop accum s = do
            s <- g s
            case s of
                Done _ -> pure accum
                Yield s' a ->
                    let !accum' = f accum a
                     in loop accum' s'
                Skip s' -> loop accum s'
{-# INLINE foldlS #-}

sumS :: (Monad m, Num a)
     => StreamT a m r
     -> m a
sumS = foldlS (+) 0
{-# INLINE sumS #-}

sinkListS :: Monad m
          => StreamT i m r
          -> m [i]
sinkListS =
    fmap ($ []) . foldlS go id
  where
    go front x = front . (x:)
{-# INLINE sinkListS #-}

mapM_S :: Monad m
       => (i -> m a)
       -> StreamT i m r
       -> m ()
mapM_S f =
    makeSink go
  where
    go s0 g =
        let loop s = do
                step <- g s
                case step of
                    Done _ -> pure ()
                    Yield s' i -> f i *> loop s'
                    Skip s' -> loop s'
         in loop s0
{-# INLINE mapM_S #-}

readFileS :: (Catch.MonadMask m, MonadIO m)
          => FilePath
          -> StreamT S.ByteString m ()
readFileS fp = makeSourceWith
    (Catch.bracket
        (liftIO (FR.openFile fp))
        (liftIO . FR.closeFile))
    go
  where
    go h =
        liftIO $ fmap toStep $ FR.readChunk h
      where
        toStep bs
            | S.null bs = Done ()
            | otherwise = Yield h bs
{-# INLINE readFileS #-}

writeFileS :: (Catch.MonadMask m, MonadIO m)
           => FilePath
           -> StreamT S.ByteString m ()
           -> m ()
writeFileS fp stream = Catch.bracket
    (liftIO $ IO.openBinaryFile fp IO.WriteMode)
    (liftIO . IO.hClose)
    (\h -> mapM_S (liftIO . S.hPut h) stream)
{-# INLINE writeFileS #-}

linesAsciiS :: Monad m
            => (accum -> StreamT S.ByteString m () -> StreamT o m accum)
            -> accum
            -> StreamT S.ByteString m r
            -> StreamT o m accum
linesAsciiS inner accum0 =
    makeTransformer accum0 go
  where
    go accum s f = do
        step <- f s
        case step of
            Done _ -> pure (Done accum)
            Skip s' -> pure (Skip (accum, s'))
            Yield s' bs
                | S.null bs -> pure (Skip (accum, s'))
                | otherwise -> error "FIXME maybe this isn't possible after all"
{-# INLINE linesAsciiS #-}
