
-- | RVM regression model
module Numeric.BMML.RVMR
    ( fit
    , predict
    ) where

import Numeric.LinearAlgebra.Data (Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix as H

data RVMRModel = RVMRModel
    { mean :: Vector Double
    , covariance :: Matrix Double
    }

initAlpha :: Int -> Vector Double
initAlpha m = H.konst 1.0e-3 m

initBeta :: Double
initBeta = 1.0e-3

fit :: Matrix Double -> Vector Double -> RVMRModel
fit x t =
    let (_,d) = H.size x
        alpha = initAlpha d
        beta = initBeta
        sigma = calcSigma alpha beta x
        m = calcMean beta sigma x t
    in go 20 alpha beta sigma m
  where
    go 0 _ _ sigma m =
        RVMRModel
        { mean = m
        , covariance = sigma
        }
    go k alpha beta sigma m = go (k - 1) na nb ns nm
      where
        (n,d) = H.size x
        gamma :: Vector Double
        gamma = (H.konst 1 d) - (alpha * (H.takeDiag sigma))
        na :: Vector Double
        na =
            H.fromList
                (zipWith
                     (\gi mi ->
                           gi / (mi * mi))
                     (H.toList gamma)
                     (H.toList m))
        nb :: Double
        nb = normSq (t - x H.#> m) / (fromIntegral n - H.sumElements gamma)
        ns = calcSigma na nb x
        nm = calcMean nb ns x t

calcSigma :: Vector Double -> Double -> Matrix Double -> Matrix Double
calcSigma a b x = H.inv (H.diag a + b `H.scale` (H.tr x H.<> x))

calcMean :: Double
         -> Matrix Double
         -> Matrix Double
         -> Vector Double
         -> Vector Double
calcMean b s x t = b `H.scale` ((s H.<> H.tr x) H.#> t)

normSq :: Vector Double -> Double
normSq v = v `H.dot` v

predict :: RVMRModel -> Vector Double -> Double
predict (RVMRModel m _) v = m `H.dot` v
