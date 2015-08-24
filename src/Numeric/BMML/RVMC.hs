
{- | Relevant Vector Machine. Classifier -}
module Numeric.BMML.RVMC
    ( fit
    , predict
    ) where

import           Numeric.LinearAlgebra.Data (Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix as H

data RVMCModel = RVMCModel
    { mean :: Vector Double
    , covariance :: Matrix Double
    }


initAlpha :: Int -> Vector Double
initAlpha = H.konst 1.0e-3

initY :: Int -> Vector Double
initY = H.konst 0

calcB :: Vector Double -> Matrix Double
calcB y =
    H.diag
        (H.cmap
             (\yi ->
                   yi * (1 - yi))
             y)

calcSigma :: Matrix Double -> Matrix Double -> Vector Double -> Matrix Double
calcSigma phi b alpha = H.inv (H.tr phi H.<> b H.<> phi + H.diag alpha)

calcW
    :: Vector Double
    -> Matrix Double
    -> Vector Double
    -> Vector Double
    -> Vector Double
calcW alpha phi t y = ((H.inv (H.diag alpha)) H.<> (H.tr phi)) H.#> (t - y)

calcY :: Vector Double -> Matrix Double -> Vector Double
calcY w phi = H.cmap sigmoid (phi H.#> w)

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

fit :: Matrix Double -> Vector Double -> RVMCModel
fit phi t =
    let (n,d) = H.size phi
        alpha = initAlpha d
        y = initY n
        b = calcB y
        sigma = calcSigma phi b alpha
    in go (200 :: Int) alpha y b (calcW alpha phi t y) sigma
  where
    go 0 _ _ _ w sigma =
        RVMCModel
        { mean = w
        , covariance = sigma
        }
    go k alpha y b w sigma = go (k-1) na ny nb nw ns
      where
        gamma = H.cmap (1 -) (alpha * (H.takeDiag sigma))
        na = gamma / (w * w)
        ny = calcY w phi
        nb = calcB ny
        ns = calcSigma phi nb na
        nw = calcW na phi t ny


-- | Return probability of phi is from C_1 (t = 1)
predict :: RVMCModel -> Vector Double -> Double
predict (RVMCModel w s) phi = sigmoid ((k sigma_a) * m_a)
  where
    m_a = w `H.dot` phi
    sigma_a = phi `H.dot` (s H.#> phi)
    k x = sqrt (1 + 3 * x / 8)

