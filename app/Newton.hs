module Newton
 (
    printNewton
 )where

import Data
import Util

dy = [y] ++ (generateDy y)

newton :: Double -> [Double] -> [Double] -> Double
newton x' x y = firstY + (newtonIteration x' (IterationState 0 1 (secondX - fistsX)) x (firstElemenetFromListOfList$generateDy$y) )where
    (fistsX:(secondX:_)) = x
    (firstY:_)=y

newtonIteration ::  Double -> IterationState -> [Double] -> [Double] -> Double
newtonIteration _ _ [] _ = 0
newtonIteration _ _ _ [] = 0
newtonIteration x' state (x:xTail) (dy:dyTail) = (dy * newX) +
 newtonIteration x' (IterationState newN newX (stepValue state)) xTail dyTail where
  newN = (n state) +1
  newX = ((x'- x) * (buf state ))/(newN * (stepValue state))

printNewton::[Double] -> [Double] ->[Double]->IO()
printNewton _ _ [] = do putStrLn "\n"
printNewton x y (head:tail) = do
    putStr $ show $ (newton head x y)
    putStr "\t"
    printNewton x y tail