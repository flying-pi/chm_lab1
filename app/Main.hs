module Main where

import Prelude
import Lagrange
import Data
import Newton
import Hause

import Util

main :: IO ()
main = do
    putStr "Лагранж ::\n\t"
    printLagrangeResult valueTable xTi
    putStr "Н'ютон ::\n\t"
    printNewton x y xTi
    putStr "Гаус ::\n\t"
    printHause x y xTi
