{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Sink where

import qualified Data.ByteString as S
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Streaming.FileRead as FR
import qualified System.IO as IO

data SinkT i m r = forall s. SinkT
    (s -> i -> m (Either s r))
    (s -> m r)
    (forall b. (s -> m b) -> m b)

foldlSi :: Monad m
        => (accum -> i -> accum)
        -> accum
        -> SinkT i m accum
foldlSi f accum0 =
    SinkT f' return ($ accum0)
  where
    f' accum i =
        return $ Left accum'
      where
        !accum' = f accum i
{-# INLINE foldlSi #-}

sumSi :: (Monad m, Num a) => SinkT a m a
sumSi = foldlSi (+) 0
{-# INLINE sumSi #-}

mapSi :: Monad m
      => (i -> o)
      -> SinkT o m r
      -> SinkT i m r
mapSi mapper (SinkT f close withAccum) =
    SinkT go close withAccum
  where
    go accum i = f accum (mapper i)
{-# INLINE mapSi #-}

enumFromToSi :: Monad m
             => Int
             -> Int
             -> SinkT Int m r
             -> m r
enumFromToSi low high (SinkT f close withAccum) =
    withAccum (go low)
  where
    go i accum
        | i <= high = do
            res <- f accum i
            case res of
                Left accum' -> go (i + 1) accum'
                Right res -> return res
        | otherwise = close accum
{-# INLINE enumFromToSi #-}

readFileSi :: (Catch.MonadMask m, MonadIO m)
           => FilePath
           -> SinkT S.ByteString m r
           -> m r
readFileSi fp (SinkT f close withAccum) =
    (Catch.bracket
        (liftIO (FR.openFile fp))
        (liftIO . FR.closeFile))
        go
  where
    go h =
        withAccum loop
      where
        loop accum = do
            bs <- liftIO $ FR.readChunk h
            if not $ S.null bs
                then do
                    res <- f accum bs
                    case res of
                        Left accum' -> loop accum'
                        Right res -> return res
                else close accum
{-# INLINE readFileSi #-}

writeFileSi :: (Catch.MonadMask m, MonadIO m)
            => FilePath
            -> SinkT S.ByteString m ()
writeFileSi fp =
    SinkT
        go
        (const $ return ())
        (Catch.bracket
            (liftIO $ IO.openBinaryFile fp IO.WriteMode)
            (liftIO . IO.hClose))
  where
    go h bs = do
        liftIO $ S.hPut h bs
        return $ Left h
{-# INLINE writeFileSi #-}
