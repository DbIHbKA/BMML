
import qualified Numeric.BMML.RVMR             as RVM
import qualified Numeric.LinearAlgebra.HMatrix as H
import           System.Random.MWC             (asGenIO, uniformVector,
                                                withSystemRandom)

f :: Double -> Double
f x = 2.0 * x


generateX :: IO (H.Vector Double)
generateX =
    withSystemRandom . asGenIO $
    \gen ->
         uniformVector gen 12500

generateT :: H.Vector Double -> H.Vector Double
generateT = H.cmap f

main :: IO ()
main = do
  x <- generateX
  noiseX <- H.rand 12500 2
  let trainT = generateT x
      trainX = (H.fromColumns [x]) H.||| noiseX
  print (f 0.5)
  print (RVM.predict (RVM.fit trainX trainT) (H.fromList [0.5, 10.0, 10000.0]))
