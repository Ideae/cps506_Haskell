module Assign1 where
    trifecta :: (a -> b) -> (b -> c) -> (c -> d) -> a -> d 
    trifecta x y z w = z (y (x w))

    mapCF :: a -> (a -> b -> c) -> [b] -> [c]
    mapCF x y z = map w z where w = y x

    allPairFunc :: [a -> b -> Bool] -> [(a,b)] -> Bool
    allPairFunc [][] = True
    allPairFunc (x:y) (z:w) | (length y) /= (length w) = False
                            | otherwise = x ((fst z) (snd z)) && allPairFunc y w


    asBool :: Num -> Bool
    asBool i | i == 0 = False
             | otherwise True