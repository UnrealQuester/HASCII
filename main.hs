import Control.Applicative
import Vision.Image
import Vision.Primitive
import qualified System.Console.Terminal.Size as T
import System.Console.CmdTheLine

fileName :: Term String
fileName = required $ pos 0 Nothing posInfo {posName = "FILENAME", posDoc = "path to the file to be converted to ascii"}

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

termInfo :: TermInfo
termInfo = defTI { termName = "HASCII", version = "1.0"  }

printAscii :: String -> IO ()
printAscii file = do
    terminalSize <- T.size
    let imageWidth = maybe 75 T.width terminalSize
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
term = printAscii <$> fileName

main :: IO ()
main = do
    run (term, termInfo)
