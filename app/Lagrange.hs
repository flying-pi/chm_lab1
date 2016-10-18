module Lagrange
 (
    printLagrangeResult
 )where

import Prelude
import Util

lagrange::[(Double,Double)]->Double->Double
lagrange valueTable x' =forEach polinom 0 0 valueTable
    where polinom =(\init (x,y) j -> init + y * (forEach (\initB (xb,yb) i -> (if i == j then initB else initB*((x'-xb)/(x-xb)))) 1 0 valueTable))

printLagrangeResult :: [(Double,Double)]->[Double]->IO()
printLagrangeResult _ [] = do putStrLn "\n"
printLagrangeResult valueTable (head:tail) = do
    putStr(show(lagrange valueTable head))
    putStr "\t"
    printLagrangeResult valueTable tail