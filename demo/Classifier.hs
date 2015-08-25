{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import Frames
import Frames.CSV (rowGen, RowGen(..), readTableOpt)
import Pipes (Producer)
import qualified Data.Foldable as F
import qualified Numeric.LinearAlgebra.HMatrix as H

import Numeric.BMML.RVMC


tableTypes'
    rowGen
    { rowTypeName = "Iris"
    , columnNames = [ "sepal length"
                    , "sepal width"
                    , "petal length"
                    , "petal width"
                    , "iris name"]
    }
    "data/iris.csv"

irisStream :: Producer Iris IO ()
irisStream = readTableOpt irisParser "data/iris.csv"

loadIris :: IO (Frame Iris)
loadIris = inCoreAoS irisStream

restructureIris :: ( CanDelete IrisName rs
                   , rs' ~ RDelete IrisName rs
                   , AllAre Double (UnColumn rs')
                   , AsVinyl rs') => Record rs -> Record (IrisName ': rs')
restructureIris r = frameCons (rget' irisName' r) (rdel [pr|IrisName|] r)

splitXY :: (AllAre Double (UnColumn rs), AsVinyl rs)
        => Record (s :-> Text ': rs) -> (Text, [Double])
splitXY (recUncons -> (h, t)) = (h, recToList t)

loadDataSet :: IO ([Text], [[Double]])
loadDataSet =
    loadIris >>=
    \ds ->
         return $ unzip $ F.foldMap ((: []) . splitXY . restructureIris) ds


splitTrainTest :: [[Double]]
               -> [Text]
               -> (([[Double]], [Double]), ([[Double]], [Double]))
splitTrainTest x y =
    splitTT
        (takeN
             (foldr
                  (\(xi,yi) ((x1,y1),(x0,y0)) ->
                        if yi == "Iris-setosa"
                            then ((x1 ++ [xi], y1 ++ [1]), (x0, y0))
                            else ((x1, y1), (x0 ++ [xi], y0 ++ [0])))
                  (([], []), ([], []))
                  (zip x y)))
  where
    takeN ((x1,y1),(x0,y0)) = ((take n x1, take n y1), (take n x0, take n y0))
      where
        n = min (length y1) (length y0)
    splitTT ((x1,y1),(x0,y0)) = ((x1Train ++ x0Train, y1Train ++ y0Train), (x1Test ++ x0Test, y1Test ++ y0Test))
      where
        n = length y1
        m = floor (0.7 * fromIntegral n)
        (x1Train,x1Test) = splitAt m x1
        (y1Train,y1Test) = splitAt m y1
        (x0Train,x0Test) = splitAt m x0
        (y0Train,y0Test) = splitAt m y0

main :: IO ()
main = do
    (y,x) <- loadDataSet
    let ((xTrain,yTrain),(xTest,yTest)) = splitTrainTest x y
        c = fit (H.fromLists xTrain) (H.fromList yTrain)
    mapM_
        (\(x,y) ->
              print
                  ("Real: " ++
                   show y ++
                   "probability of class 1: " ++
                   show (predict c (H.fromList x))))
        (zip xTest yTest)
