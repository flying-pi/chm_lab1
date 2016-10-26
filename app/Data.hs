module Data
 (
    module Data
 ) where

b = 4.8

y = [8.6,-12.2,-2.8,2.7,-5.3]

x = [k*b|k <- [0,0.25,0.5,0.75,1]]

valueTable ::  [(Double,Double)]
valueTable = zip x y

xTi = map (\(x, _) -> if(x +b/8 > b)then b else x +b/8 ) valueTable



test = zip [0,1 .. 9] [k*k|k<-[0,1 .. 9]]

yTest :: [Double]
yTest = [k*k |k<-xTest]

fac::Double->Double
fac v = if(v<=0) then 1 else (v*(v - 1))

xTest = [0,1 .. 9]
