
module Numeric.BMML.Regression
    (
    ) where

import Numeric.LinearAlgebra.Data (Matrix, Vector)


-- | Linear Regression Model
data LRModel = LRM
    { mN :: Vector Double
    , sN :: Matrix Double
    , gamma :: Double
    , alpha :: Double
    , beta :: Double
    }

fit :: Matrix Double -> LRModel
fit = undefined

predict :: LRModel -> Vector Double -> Double
predict = undefined
