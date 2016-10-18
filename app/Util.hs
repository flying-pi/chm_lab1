module Util where


forEach :: (Num i) => (a->b->i->a)->a->i->[b]->a
forEach _ init _ [] = init
forEach fun init iterator (head:tail) = forEach fun (fun init head iterator) (iterator + 1) tail

fixpoint f x = f (fixpoint f) x

firstElemenetFromListOfList :: [[a]] -> [a]
firstElemenetFromListOfList [] = []
firstElemenetFromListOfList ((first:_):tail) = [first] ++ (firstElemenetFromListOfList tail)

generateDy::[Double] -> [[Double]]
generateDy (head:[]) = []
generateDy source = [newCollumn] ++ (generateDy newCollumn)
 where newCollumn = delta source

delta :: [Double] -> [Double]
delta previesIterration = let (y1:tail) = previesIterration
                              (_:(y2:_)) = previesIterration
                          in if((length previesIterration)<2) then [] else [y2-y1] ++ (delta tail)

data IterationState = IterationState {
    n :: Double,
    buf :: Double,
    stepValue :: Double
}