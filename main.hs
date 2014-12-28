import System.Environment (getArgs)
import Vision.Image
import Vision.Primitive
import System.Console.Terminal.Size

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

asciiChars = [ '#', '?', '%', 'S', '@', '+', '*', ':', ',', '.']

pixelToAscii :: GreyPixel -> Char
pixelToAscii pixel = asciiChars !! (floor $ aspectRatio * fromIntegral pixel)
    where
        aspectRatio = (fromIntegral $ length asciiChars) / (256) :: Double

toAsciiRow :: Grey -> Int -> String
toAsciiRow img y = fmap (\x -> pixelToAscii (index img (Z:. y :. x)) ) [0 .. (imgWidth img) - 1]

toAscii :: RGB -> [String]
toAscii img = fmap (toAsciiRow greyscale) (reverse [0 .. (imgHeight greyscale) - 1]) where
    greyscale = convert img :: Grey

printLines :: [String] -> IO ()
printLines l = mapM_ putStrLn l

main :: IO ()
main = do
    [input] <- getArgs
    terminalSize <- size
    let imageWidth = maybe 75 width terminalSize
    imgage <- load Nothing input
    case imgage of
        Right img -> do
            let
                rgb = convert img ::RGB
                miniature = fitToWidth imageWidth rgb
                asciiArt = toAscii miniature
            printLines asciiArt
        Left err -> print err
