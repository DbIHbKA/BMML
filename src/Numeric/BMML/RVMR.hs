
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
    , optbeta :: Double
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
    in go 10000 alpha beta sigma m
  where
    go 0 _ beta sigma m =
        RVMRModel
        { mean = m
        , covariance = sigma
        , optbeta = beta
        }
    go k alpha beta sigma m = if sNorm alpha na < 0.01
                                 then go 0 na nb ns nm
                                else go (k - 1) na nb ns nm
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

sNorm :: Vector Double -> Vector Double -> Double
sNorm x y =
    sqrt
        (foldr
             (\(xi,yi) res ->
                   res + (yi - xi) * (yi - xi))
             0
             (filter
                  (\(xi,yi) ->
                        xi < 10000 && yi < 10000)
                  (zip (H.toList x) (H.toList y))))
{-# INLINE sNorm #-}

calcSigma :: Vector Double -> Double -> Matrix Double -> Matrix Double
calcSigma a b x = H.inv (H.diag a + b `H.scale` (H.tr x H.<> x))
{-# INLINE calcSigma #-}

calcMean :: Double
         -> Matrix Double
         -> Matrix Double
         -> Vector Double
         -> Vector Double
calcMean b s x t = b `H.scale` ((s H.<> H.tr x) H.#> t)
{-# INLINE calcMean #-}

normSq :: Vector Double -> Double
normSq v = v `H.dot` v
{-# INLINE normSq #-}

predict :: RVMRModel -> Vector Double -> (Double, Double)
predict (RVMRModel m sigma ob) v =
    (m `H.dot` v, 1 / ob + v `H.dot` (sigma H.#> v))
