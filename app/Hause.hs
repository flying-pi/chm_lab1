module Hause
 (
    printHause,
    hause
 )where

import Util

hauseY :: [[Double]]->[Double]
hauseY [] = []
hauseY (head:tail) = [head !!  ((quot ((length head) + (if (mod 2 (length head)) == 2 then 1 else 0)) 2))] ++ (hauseY tail)

hauseX :: [Double] ->[Double]
hauseX [] = []
hauseX input = [input !! currentX] ++ (hauseX (let (ys,zs) = splitAt currentX input   in   ys ++ (tail zs)))
 where currentX = (quot ((length input) + (if (mod 2 (length input)) == 2 then 1 else 0)) 2)

hause :: Double -> [Double] -> [Double] -> Double
hause x' x y = (y!!currentY) +(hauseIteration x' (IterationState 0 1 (secondX - fistsX)) (hauseX x) (hauseY$generateDy$y) ) where
                           (fistsX:(secondX:_)) = x
                           currentY = (quot ((length y) + (if (mod 2 (length y)) == 2 then 1 else 0)) 2)

hauseIteration ::  Double -> IterationState -> [Double] -> [Double] -> Double
hauseIteration _ _ [] _ = 0
hauseIteration _ _ _ [] = 0
hauseIteration x' state (x:xTail) (dy:dyTail) = (dy * newX) +
 hauseIteration x' (IterationState newN newX (stepValue state)) xTail dyTail where
  newN = (n state) +1
  newX = ((x'- x) * (buf state ))/(newN * (stepValue state))


printHause::[Double] -> [Double] ->[Double]->IO()
printHause _ _ [] = do putStrLn "\n"
printHause x y (head:tail) = do
    putStr $ show $ (hause head x y)
    putStr "\t"
    printHause x y tail