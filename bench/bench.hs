import Criterion.Main
import Otsu
import Data.Vector.Storable as V

vec :: V.Vector Double
vec = V.map (/s) (V.enumFromN 1 256) where s = Prelude.sum [1..256]

main :: IO ()
main = defaultMain [
  bgroup "multiOtsu"
    [ bench "3"  $ nf (multiOtsu vec) 3
    , bench "2"  $ nf (multiOtsu vec) 2
    , bench "1"  $ nf (multiOtsu vec) 1
    ]
  ]
