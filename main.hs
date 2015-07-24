{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (map, concatMap, sum, reverse, length, head, lookup)
import qualified Data.Vector.Storable as V
import Control.Applicative
import Control.Arrow
import Vision.Image hiding (map)
import Vision.Image.Storage.DevIL
import Vision.Primitive
import qualified Vision.Histogram as H
import qualified System.Console.Terminal.Size as T
import Data.List.Stream
import Data.Ord (comparing)
import qualified Data.ByteString as BS
import Options.Applicative hiding(helper)

data FitMode = FitToWidth | FitToHeight | FitToSmallest | Original deriving (Eq)

data CmdOptions = CmdOptions
    { argFileName :: Maybe FilePath
    , argHeight   :: Maybe Int
    , argWidth    :: Maybe Int
    , argFitMode  :: FitMode
    , argSymbols  :: String }

parseOptions :: Parser CmdOptions
parseOptions = CmdOptions
    <$> parseFilename
    <*> parseHeight
    <*> parseWidth
    <*> parseFitMode
    <*> parseSymbols

parseFilename :: Parser (Maybe String)
parseFilename = optional $ argument str (metavar "FILENAME" <> help "Path to the file to be converted to ascii")

helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , help "Show this help text"
  , hidden ]

parseHeight :: Parser (Maybe Int)
parseHeight = optional $ option auto (long "height" <> short 'h' <> metavar "HEIGHT" <> help "Height of the output in characters")

parseWidth :: Parser (Maybe Int)
parseWidth = optional $ option auto (long "width" <> short 'w' <> metavar "WIDTH" <> help "Width of the output in characters")

parseSymbols :: Parser String
parseSymbols = option auto (long "characters" <> short 'c' <> value (reverse "█▓▒░") <> metavar "CHARACTERS" <> help "What characters to use for the image")

fitModes :: String -> ReadM FitMode
fitModes s = do
    let modes = [("width", FitToWidth), ("height", FitToHeight), ("original", Original), ("smallest", FitToSmallest)]
    let val = lookup s modes
    case val of
      Nothing -> readerError "Not a valid mode"
      Just x -> return x

parseFitMode :: Parser FitMode
parseFitMode = option
    (str >>= fitModes)
    (value FitToWidth <> long "mode" <> short 'm' <> metavar "width|height|original|smallest" <> help "How the image should be resized")

range :: Int -> [(Int, Int)] -> [[(Int, Int)]]
range 0 xs = [xs]
range n xs = concatMap (range (n-1) . splitRange xs) ((rangePair . head) xs)

rangePair :: (Int, Int) -> [Int]
rangePair p = [fst p .. snd p - 1]

splitRange :: [(Int, Int)] -> Int -> [(Int, Int)]
splitRange [] _ = []
splitRange (x:xs) n = (fst x, n) : (n + 1, snd x) : xs

sigma :: H.Histogram DIM1 Double -> [(Int, Int)] -> Double
sigma hist thresh = sum $ uncurry interClassVariance <$> thresh
    where
        histVec = H.vector hist
        interClassVariance u v | puv == 0 = 0
                               | otherwise = s u v ^ (2::Integer) / puv where
                                   puv = p u v
        lS = (sumVec V.!)
        multVec = V.imap (\i x -> fromIntegral (i+1) * x) histVec
        sumVec = V.postscanl (+) 0 multVec
        lP = (V.postscanl (+) 0 histVec V.!)
        p u v = lP v - lP u
        s u v = lS v - lS u

multiOtsu :: H.Histogram DIM1 Double -> Int -> [Int]
multiOtsu hist n = fst $ maximumBy (comparing snd) $ map ((<$>) snd &&& sigma hist) (range n [(0, 255)])

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
pixelToAscii pix hist = pixelToAscii' (multiOtsu gnorm 3)
    where
        pixelToAscii' _ [] = ' '
        pixelToAscii' [] (c:_) = c
        pixelToAscii' (x:xs) (c:cs) = if fromIntegral pix <= x then c else pixelToAscii' xs cs
        gnorm = H.normalize 1.0 hist

toAsciiRow :: Grey -> String -> Int -> String
toAsciiRow img asciiChars y = map (\x -> pixelToAscii (index img (Z:. y :. x)) (H.histogram Nothing img :: H.Histogram DIM1 Int) asciiChars) [0 .. imgWidth img - 1]

toAscii :: RGB -> String -> [String]
toAscii img asciiChars = map (toAsciiRow greyscale asciiChars) [0 .. imgHeight greyscale - 1] where
    greyscale = convert img :: Grey

printLines :: [String] -> IO ()
printLines = mapM_ putStrLn

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

readImageInput :: IO (Either StorageError RGB)
readImageInput = loadBS Autodetect <$> BS.getContents

readImageFile :: FilePath -> IO (Either StorageError RGB)
readImageFile = load Autodetect

printAscii :: CmdOptions -> IO ()
printAscii options = do
    transform <- outPutSize (argWidth options) (argHeight options) (argFitMode options)
    imgage <- maybe readImageInput readImageFile (argFileName options)
    case imgage of
        Right (img :: RGB) -> do
            let
                miniature = (convert . transform) img
                asciiArt = toAscii miniature (argSymbols options)
            printLines asciiArt
        Left err -> print err

main :: IO ()
main = execParser opts >>= printAscii where
    opts = info (helper <*> parseOptions) (fullDesc)
