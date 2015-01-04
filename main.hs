import Debug.Trace
import qualified Data.Vector.Storable as V
import Control.Applicative
import Vision.Image hiding (map)
import Vision.Primitive
import qualified Vision.Histogram as H
import qualified System.Console.Terminal.Size as T
import System.Console.CmdTheLine

fileName :: Term String
fileName = required $ pos 0 Nothing posInfo {posName = "FILENAME", posDoc = "path to the file to be converted to ascii"}

argWidth :: Term (Maybe Int)
argWidth = value $ opt Nothing (optInfo ["w", "width"]) {optName = "WIDTH", optDoc = "Width of the output in characters"}

fitToWidth :: Int -> RGB -> RGB
fitToWidth width img = resize TruncateInteger (ix2 height width) img
    where
        height = ceiling $ (aspectRatio) * (fromIntegral width) :: Int
        aspectRatio = (fromIntegral $ imgHeight img) / (fromIntegral $ imgWidth img) :: Double

imgWidth :: MaskedImage i => i -> Int
imgWidth img = imgWidth' $ shape img
    where
        imgWidth' (Z :. _ :. w) = w

imgHeight :: MaskedImage i => i -> Int
imgHeight img = imgHeight' $ shape img
    where
        imgHeight' (Z :. h :. _) = h

asciiChars :: [Char]
asciiChars = "█▓▒░#%@+*:,,., "

divideEvenly :: Double -> [Int] -> [[Int]]
divideEvenly _    [] = [[]]
divideEvenly size xs = bucket : (divideEvenly size remaining)
    where
        (bucket, remaining) = (take n xs, drop n xs)
        n = n' $ dropWhile (\l -> fromIntegral (sum (take l xs)) < size) [1 .. length xs]
        n' [] = length xs
        n' (x:_) = x

asciiHist :: H.Histogram DIM1 Int -> [Char]
asciiHist hist = concat $ zipWith (\a b -> map (const a) b) asciiChars $ (divideEvenly bucketSize (histList hist))
    where
       histList (H.Histogram _ vec) = V.toList vec
       bucketSize = (fromIntegral $ sum (histList hist)) / (fromIntegral $ length asciiChars) :: Double

pixelToAscii :: GreyPixel -> H.Histogram DIM1 Int -> Char
pixelToAscii pixel hist = asciiHist hist !! (fromIntegral pixel)

toAsciiRow :: Grey -> Int -> String
toAsciiRow img y = fmap (\x -> pixelToAscii (index img (Z:. y :. x)) (H.histogram Nothing img :: H.Histogram DIM1 Int)) [0 .. (imgWidth img) - 1]

toAscii :: RGB -> [String]
toAscii img = fmap (toAsciiRow greyscale) (reverse [0 .. (imgHeight greyscale) - 1]) where
    greyscale = convert img :: Grey

printLines :: [String] -> IO ()
printLines l = mapM_ putStrLn l

termInfo :: TermInfo
termInfo = defTI { termName = "HASCII", version = "1.0"  }

printAscii :: Maybe Int -> String -> IO ()
printAscii asciiWidth file = do
    terminalSize <- T.size
    let imageWidth = case asciiWidth of
            Nothing -> maybe 75 T.width terminalSize
            Just w -> w
    imgage <- load Nothing file
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
main = do
    run (term, termInfo)
