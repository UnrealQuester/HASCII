module Otsu (
    multiOtsu
    ) where

import Prelude hiding (map, concatMap, sum, reverse, length, head, lookup)
import qualified Data.Vector.Storable as V
import Data.Ord (comparing)
import Data.List.Stream
import Control.Arrow
import Control.Applicative

range :: Int -> [(Int, Int)] -> [[(Int, Int)]]
range 0 xs = [xs]
range n xs = concatMap (range (n-1) . splitRange xs) ((rangePair . head) xs)

rangePair :: (Int, Int) -> [Int]
rangePair p = [fst p .. snd p - 1]

splitRange :: [(Int, Int)] -> Int -> [(Int, Int)]
splitRange [] _ = []
splitRange (x:xs) n = (fst x, n) : (n + 1, snd x) : xs

sigma :: V.Vector Double -> [(Int, Int)] -> Double
sigma histVec thresh = sum $ uncurry interClassVariance <$> thresh
    where
        interClassVariance u v | puv == 0 = 0
                               | otherwise = s u v ^ (2::Integer) / puv where
                                   puv = p u v
        lS = (sumVec V.!)
        multVec = V.imap (\i x -> fromIntegral (i+1) * x) histVec
        sumVec = V.postscanl (+) 0 multVec
        lP = (V.postscanl (+) 0 histVec V.!)
        p u v = lP v - lP u
        s u v = lS v - lS u

multiOtsu :: V.Vector Double -> Int -> [Int]
multiOtsu histVec n = fst $ maximumBy (comparing snd) $ map ((<$>) snd &&& sigma histVec) (range n [(0, 255)])
