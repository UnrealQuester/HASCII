module Otsu (
    multiOtsu
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Ord (comparing)
import Control.Arrow
import Control.Applicative

range :: Int -> [(Int, Int)] -> [[(Int, Int)]]
range 0 xs = [xs]
range n xs = (range (n-1) . splitRange xs) =<< ((rangePair . head) xs)

pixelRange :: Int -> V.Vector [(Int, Int)]
pixelRange n = V.fromList $ range n [(0, 255)]

rangePair :: (Int, Int) -> [Int]
rangePair p = [fst p .. snd p - 1]

splitRange :: [(Int, Int)] -> Int -> [(Int, Int)]
splitRange [] _ = []
splitRange (x:xs) n = (fst x, n) : (n + 1, snd x) : xs


sigma :: U.Vector Double -> [(Int, Int)] -> Double
sigma histVec thresh = sum $ (uncurry interClassVariance) <$> thresh
    where
        interClassVariance u v | puv == 0 = 0
                               | otherwise = s u v ^ (2::Int) / puv where
                                    puv = p u v
        multVec = U.imap (\i x -> fromIntegral (i+1) * x) histVec
        sumVec = U.postscanl (+) 0 multVec
        lP = U.postscanl (+) 0 histVec
        p  u v = lP `U.unsafeIndex` v - lP `U.unsafeIndex` u
        s  u v = sumVec `U.unsafeIndex` v - sumVec `U.unsafeIndex` u

bestCandidate :: Ord b => (a -> b) -> V.Vector a -> a
bestCandidate f xs = fst . V.maximumBy (comparing snd) $ V.map (id &&& f) xs

multiOtsu :: U.Vector Double -> Int -> [Int]
multiOtsu histVec n = snd <$> bestCandidate (sigma histVec) (pixelRange n)
