module Main where

import Prelude
import Lagrange
import Data
import Newton
import Hause

import Util

import Graphics.UI.GLUT

main :: IO ()
main = do
    putStr "source :: "
    putStr (show xTi)
    putStr "\nЛагранж ::\n\t"
    printLagrangeResult valueTable xTi
    putStr "Н'ютон ::\n\t"
    printNewton x y xTi
    putStr "Гаус ::\n\t"
    printHause x y xTi
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "lab #1"
    displayCallback $= display
    mainLoop


display :: DisplayCallback
display = do
  clear [ColorBuffer]
  color3f 0.4 0.4 0.4
  renderPrimitive Lines $ do
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) template
  color3f 1 1 1
  renderPrimitive LineLoop $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush


step = 0.05
size = 0.01

template :: [(Double,Double,Double)]
template = [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0)] ++
   concat [[(k,-size,0),(k,size,0),(-size,k,0),(size,k,0)] | k<-[-1,-1 + step .. 1]]

label :: Double -> Double -> [(Double,Double,Double)]
label position step = [(position,-step,0),(position,step,0),(step,position,0),(-step,position,0)]

myPoints :: [(Double,Double,Double)]
myPoints = map (\(x, y, z) -> ( x * step, y * step,z * step)) [( k , (hause k x y) , 0) | k <- [-(1/step),-(1/step) + step .. (1/step)]]