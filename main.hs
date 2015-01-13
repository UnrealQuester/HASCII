import qualified Data.Vector.Storable as V
import Control.Applicative
import Vision.Image hiding (map)
import Vision.Primitive
import qualified Vision.Histogram as H
import qualified System.Console.Terminal.Size as T
import System.Console.CmdTheLine
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe
import qualified Data.ByteString as BS

fileName :: Term (Maybe String)
fileName = value $ pos 0 Nothing posInfo {posName = "FILENAME", posDoc = "path to the file to be converted to ascii"}

argWidth :: Term (Maybe Int)
argWidth = value $ opt Nothing (optInfo ["w", "width"]) {optName = "WIDTH", optDoc = "Width of the output in characters"}

addTo :: a -> [[a]] -> [[a]]
addTo x = fmap (x:)

thresholds :: Int -> Int -> [[Int]]
thresholds 1 m = fmap (:[]) [m+1 .. 254]
thresholds n m = concatMap (\x -> addTo x (thresholds (n-1) x)) [m+1 .. 255-n]

toRange :: [Int] -> [(Int, Int)]
toRange ( [v]  ) = [(v, 255)]
toRange (u:v:xs) = (u, v-1) : toRange (v:xs)

sigma :: H.Histogram DIM1 Double -> [Int] -> Double
sigma hist thresh = sum $ replaceNaN $ uncurry interClassVariance <$> toRange thresh
    where
        histVec = H.vector hist
        interClassVariance u v = s u v ^ (2::Integer) / p u v
        replaceNaN = map (\x -> if isNaN x then 0 else x)
        lS = (sumVec V.!)
        multVec = V.imap (\i x -> fromIntegral (i+1) * x) histVec
        sumVec = V.postscanl (+) 0 multVec
        lP = (V.postscanl (+) 0 histVec V.!)
        p u v = lP v - lP u
        s u v = lS v - lS u


multiOtsu :: H.Histogram DIM1 Double -> Int -> [Int]
multiOtsu hist n = fst $ maximumBy (comparing snd) $ fmap (\x -> (x, sigma hist (0:x))) (thresholds n 1)

fitToWidth :: Int -> RGB -> RGB
fitToWidth width img = resize TruncateInteger (ix2 height width) img
    where
        height = ceiling $ aspectRatio * fromIntegral width * 0.5 :: Int
        aspectRatio = fromIntegral (imgHeight img) / fromIntegral (imgWidth img) :: Double

imgWidth :: MaskedImage i => i -> Int
imgWidth img = imgWidth' $ shape img
    where
        imgWidth' (Z :. _ :. w) = w

imgHeight :: MaskedImage i => i -> Int
imgHeight img = imgHeight' $ shape img
    where
        imgHeight' (Z :. h :. _) = h

asciiChars :: [Char]
asciiChars = "█▓▒░"

pixelToAscii :: GreyPixel -> H.Histogram DIM1 Int -> Char
pixelToAscii pix hist = pixelToAscii' (multiOtsu gnorm 3) asciiChars
    where
        pixelToAscii' [] (c:_) = c
        pixelToAscii' (x:xs) (c:cs) = if fromIntegral pix < x then c else pixelToAscii' xs cs
        gnorm = H.normalize 1.0 hist

toAsciiRow :: Grey -> Int -> String
toAsciiRow img y = fmap (\x -> pixelToAscii (index img (Z:. y :. x)) (H.histogram Nothing img :: H.Histogram DIM1 Int)) [0 .. imgWidth img - 1]

toAscii :: RGB -> [String]
toAscii img = fmap (toAsciiRow greyscale) (reverse [0 .. imgHeight greyscale - 1]) where
    greyscale = convert img :: Grey

printLines :: [String] -> IO ()
printLines = mapM_ putStrLn

termInfo :: TermInfo
termInfo = defTI { termName = "HASCII", version = "1.0"  }

printAscii :: Maybe Int -> Maybe String -> IO ()
printAscii asciiWidth file = do
    terminalSize <- T.size
    let imageWidth = fromMaybe (maybe 75 T.width terminalSize) asciiWidth
    imgage <- maybe (loadBS Nothing =<< BS.getContents) (load Nothing) file
    case imgage of
        Right img -> do
            let
                rgb = convert img ::RGB
                miniature = fitToWidth imageWidth rgb
                asciiArt = toAscii miniature
            printLines asciiArt
        Left err -> print err

term :: Term (IO ())
term = printAscii <$> argWidth <*> fileName

main :: IO ()
main = run (term, termInfo)
