import Prelude hiding (map, concatMap, sum, reverse, length)
import qualified Data.Vector.Storable as V
import Control.Applicative
import Vision.Image hiding (map)
import Vision.Primitive
import qualified Vision.Histogram as H
import qualified System.Console.Terminal.Size as T
import System.Console.CmdTheLine
import Data.List.Stream
import Data.Ord (comparing)
import qualified Data.ByteString as BS

data FitMode = FitToWidth | FitToHeight | FitToSmallest | Original deriving (Eq)

instance ArgVal FitMode where
    converter = enum [ ("width", FitToWidth)
                     , ("height", FitToHeight)
                     , ("original", Original)
                     , ("smallest", FitToSmallest)]

fileName :: Term (Maybe String)
fileName = value $ pos 0 Nothing posInfo {posName = "FILENAME", posDoc = "path to the file to be converted to ascii"}

argHeight :: Term (Maybe Int)
argHeight = value $ opt Nothing (optInfo ["h", "height"]) {optName = "HEIGHT", optDoc = "Height of the output in characters"}

argWidth :: Term (Maybe Int)
argWidth = value $ opt Nothing (optInfo ["w", "width"]) {optName = "WIDTH", optDoc = "Width of the output in characters"}

argFit :: Term FitMode
argFit = value $ opt FitToWidth (optInfo ["fit-mode"]) {optDoc = "How the image should be resized"}

argChars :: Term String
argChars = value $ opt (reverse "█▓▒░") (optInfo ["c", "characters"]) {optName = "CHARACTERS", optDoc = "What characters to use for the image"}

addTo :: a -> [[a]] -> [[a]]
addTo x = map (x:)

ranges :: Int -> Int -> [[(Int, Int)]]
ranges start 1 = map (:[]) $ (,) start <$> [start .. 255]
ranges start 2 = concatMap (\x -> addTo x [[((snd x) + 1, 255)]]) $ (,) start <$> [start .. 256 - 2]
ranges start n = concatMap (\x -> addTo x (ranges ((snd x) + 1) (n - 1))) $ (,) start <$> [start .. 256 - n]

sigma :: H.Histogram DIM1 Double -> [(Int, Int)] -> Double
sigma hist thresh = sum $ replaceNaN $ uncurry interClassVariance <$> thresh
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
multiOtsu hist n = fst $ maximumBy (comparing snd) $ map (\x -> (snd <$> x, sigma hist x)) (ranges 0 n)

fitToWidth :: Int -> RGB -> RGB
fitToWidth width img = resize TruncateInteger (ix2 height width) img
    where
        height = ceiling $ aspectRatio * fromIntegral width * 0.5 :: Int
        aspectRatio = fromIntegral (imgHeight img) / fromIntegral (imgWidth img) :: Double

fitToHeight :: Int -> RGB -> RGB
fitToHeight height img = resize TruncateInteger (ix2 height width) img
    where
        width = ceiling $ aspectRatio * fromIntegral height * 2 :: Int
        aspectRatio = fromIntegral (imgWidth img) / fromIntegral (imgHeight img) :: Double

imgWidth :: MaskedImage i => i -> Int
imgWidth img = imgWidth' $ shape img
    where
        imgWidth' (Z :. _ :. w) = w

imgHeight :: MaskedImage i => i -> Int
imgHeight img = imgHeight' $ shape img
    where
        imgHeight' (Z :. h :. _) = h

pixelToAscii :: GreyPixel -> H.Histogram DIM1 Int -> String -> Char
pixelToAscii pix hist = pixelToAscii' (multiOtsu gnorm 4)
    where
        pixelToAscii' _ [] = ' '
        pixelToAscii' [] (c:_) = c
        pixelToAscii' (x:xs) (c:cs) = if fromIntegral pix <= x then c else pixelToAscii' xs cs
        gnorm = H.normalize 1.0 hist

toAsciiRow :: Grey -> String -> Int -> String
toAsciiRow img asciiChars y = map (\x -> pixelToAscii (index img (Z:. y :. x)) (H.histogram Nothing img :: H.Histogram DIM1 Int) asciiChars) [0 .. imgWidth img - 1]

toAscii :: RGB -> String -> [String]
toAscii img asciiChars = map (toAsciiRow greyscale asciiChars) (reverse [0 .. imgHeight greyscale - 1]) where
    greyscale = convert img :: Grey

printLines :: [String] -> IO ()
printLines = mapM_ putStrLn

termInfo :: TermInfo
termInfo = defTI { termName = "HASCII", version = "1.0"  }

outPutSize :: Maybe Int -> Maybe Int -> FitMode -> IO (RGB -> RGB)
outPutSize (Just w) (Just h) FitToSmallest  | w < h = outPutSize (Just w) Nothing FitToSmallest
                                            | otherwise = outPutSize Nothing (Just h) FitToSmallest
outPutSize (Just w) (Just h) _              = return $ resize TruncateInteger (ix2 h w)
outPutSize (Just w) _ _                     = return $ fitToWidth w
outPutSize _ (Just h) _                     = return $ fitToHeight h
outPutSize _ _ Original                     = return (\x -> fitToWidth (imgWidth x) x)
outPutSize _ _ FitToHeight                  = fitToHeight <$> maybe 75 T.height <$> T.size
outPutSize _ _ FitToWidth                   = fitToWidth  <$> maybe 75 T.width  <$> T.size
outPutSize _ _ FitToSmallest   = do
    size <- T.size :: IO (Maybe (T.Window Integer))
    case size of
        Nothing -> outPutSize (Just 75) Nothing Original
        Just s -> if T.width s < T.height s then
                    outPutSize Nothing Nothing FitToWidth
                  else
                    outPutSize Nothing Nothing FitToHeight


printAscii :: Maybe String -> String -> IO (RGB -> RGB) -> IO ()
printAscii file asciiChars transform = do
    imgage <- maybe (loadBS Nothing =<< BS.getContents) (load Nothing) file
    case imgage of
        Right img -> do
            trans <- transform
            let
                rgb = convert img ::RGB
                miniature = trans rgb
                asciiArt = toAscii miniature asciiChars
            printLines asciiArt
        Left err -> print err

term :: Term (IO ())
term = printAscii <$> fileName <*> argChars <*> (outPutSize <$> argWidth <*> argHeight <*> argFit)

main :: IO ()
main = run (term, termInfo)
