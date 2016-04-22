{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
import Criterion.Main
import Stream
import Sink
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import GHC.Prim
import GHC.Types
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified System.IO as IO

withTempFP :: String -> (FilePath -> IO a) -> IO a
withTempFP str inner = withSystemTempFile str $ \fp h -> do
    hClose h
    inner fp

main :: IO ()
main = withTempFP "src" $ \srcFP -> withTempFP "dst" $ \dstFP -> do
    L.writeFile dstFP $ L.fromChunks $ replicate 10000 $ S.replicate 1000 65

    defaultMain
        [ let high = 1000000 :: Int
              go name f = bench name $ whnf f high
              {-# INLINE go #-}
           in bgroup "enum/map/sum"
            [ go "sink" $ \high' ->
                runIdentity $ enumFromToSi 1 high' $ mapSi (+ 1) sumSi
            , go "stream" $ runIdentity . sumS . mapS (+ 1) . enumFromToS 1
            , go "prim" $ \(I# high') ->
                let loop x total =
                        case x +# 1# of
                            y ->
                                case total +# y of
                                    total' ->
                                        if isTrue# (x <=# high')
                                            then loop y total'
                                            else I# total'
                 in loop 1# 0#
            , go "low level" $ \high' ->
                let loop !x !total
                        | x <= high' = loop y total'
                        | otherwise = total'
                      where
                        !y = x + 1
                        !total' = total + y
                    {-# INLINE loop #-}
                 in loop 1 0
            , go "boxed vector" $ VB.sum . VB.map (+ 1) . VB.enumFromTo 1
            , go "unboxed vector" $ VU.sum . VU.map (+ 1) . VU.enumFromTo 1
            ]
        , bgroup "file copy"
            [ bench "sink" $ whnfIO $ readFileSi srcFP $ writeFileSi dstFP
            , bench "stream" $ whnfIO $ writeFileS dstFP $ readFileS srcFP
            , bench "lazy I/O" $ whnfIO $ L.readFile srcFP >>= L.writeFile dstFP
            , bench "low level" $ whnfIO $
                IO.withBinaryFile srcFP IO.ReadMode $ \src ->
                IO.withBinaryFile dstFP IO.WriteMode $ \dst ->
                    let loop = do
                            bs <- S.hGetSome src defaultChunkSize
                            if S.null bs
                                then return ()
                                else do
                                    S.hPut dst bs
                                    loop
                     in loop
            ]
        ]
