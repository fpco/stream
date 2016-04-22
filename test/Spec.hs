import Test.Hspec
import Test.Hspec.QuickCheck
import Stream
import Sink
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import System.IO.Temp
import System.IO (hClose)

main :: IO ()
main = hspec $ do
    describe "stream" $ do
        it "enum/map/sum sanity" $
            runIdentity (sumS $ mapS (+ 1) $ enumFromToS 1 (1000 :: Int))
            `shouldBe` sum (map (+ 1) [1..1000])
        prop "file copy" $ \octets ->
            withSystemTempFile "src" $ \srcFP srcH ->
            withSystemTempFile "dst" $ \dstFP dstH -> do
                let bsOrig = S.pack octets
                S.hPut srcH bsOrig
                hClose srcH
                hClose dstH
                writeFileS dstFP $ readFileS srcFP
                actual <- S.readFile dstFP
                actual `shouldBe` bsOrig
        prop "lines" $ pending {- \octetss ->
            let ls = map S.pack octetss
                bs = S8.unlines ls
                src = yieldS [bs]
                sink = linesAsciiS (const go) ()
                go = error "go"
                res = runIdentity $ sinkListS $ sink src
             in res `shouldBe` S8.lines bs
             -}

    describe "sink" $ do
        it "enum/map/sum sanity" $
            runIdentity (enumFromToSi 1 (1000 :: Int) $ mapSi (+ 1) sumSi)
            `shouldBe` sum (map (+ 1) [1..1000])
        prop "file copy" $ \octets ->
            withSystemTempFile "src" $ \srcFP srcH ->
            withSystemTempFile "dst" $ \dstFP dstH -> do
                let bsOrig = S.pack octets
                S.hPut srcH bsOrig
                hClose srcH
                hClose dstH
                readFileSi srcFP $ writeFileSi dstFP
                actual <- S.readFile dstFP
                actual `shouldBe` bsOrig
            {-
        prop "lines" $ pending {- \octetss ->
            let ls = map S.pack octetss
                bs = S8.unlines ls
                src = yieldS [bs]
                sink = linesAsciiS (const go) ()
                go = error "go"
                res = runIdentity $ sinkListS $ sink src
             in res `shouldBe` S8.lines bs
             -}
             -}
