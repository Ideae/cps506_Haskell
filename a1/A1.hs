module A1 where
    trifecta :: (a -> b) -> (b -> c) -> (c -> d) -> a -> d 
    trifecta x y z w = z (y (x w))

    mapCF :: a -> (a -> b -> c) -> [b] -> [c]
    mapCF a abc b = map abca b where abca = abc a

    allPairFunc :: [a -> b -> Bool] -> [(a,b)] -> Bool
    allPairFunc [][] = True
    allPairFunc (x:y) (z:w) | (length y) /= (length w) = False
                            | otherwise = x (fst z) (snd z) && allPairFunc y w


    asBool :: Int -> Bool
    asBool i | i == 0 = False
             | otherwise = True

    toChar :: Bool -> Char
    toChar b | b == True = 't'
             | otherwise = 'f'

    checkDoubs :: Char -> Double
    checkDoubs c | c == 't' = 1.0
                 | otherwise = 0.0

    abc :: Bool -> Int -> Double
    abc x y | x == True = fromIntegral(y * 2)
            | otherwise = fromIntegral(y)

    gt x y = x > y
    ls x y = x < y
    eq x y = x == y

    trifectaCorrect = trifecta asBool toChar checkDoubs 1 == 1.0 && trifecta asBool toChar checkDoubs 0 == 0.0
    mapCFCorrect = mapCF True abc [1..5] == [2.0,4.0,6.0,8.0,10.0] && mapCF False abc [1..5] == [1.0,2.0,3.0,4.0,5.0]
    allPairFuncCorrect = allPairFunc [gt, ls, eq] [(5,4), (3,4), (2,2)] == True && allPairFunc [gt, ls, eq] [(5,6), (3,4), (2,2)] == False && allPairFunc [ls, ls, eq] [(5,6.0), (3,4.0), (2,2.0)] == True
    
    main = do 
        print trifectaCorrect
        print mapCFCorrect
        print allPairFuncCorrect
        --print (trifecta asBool toChar checkDoubs 1)
        --print (trifecta asBool toChar checkDoubs 0)
        --print (mapCF True abc [1..5])
        --print (mapCF False abc [1..5])
        --print (allPairFunc [gt, ls, eq] [(5,4), (3,4), (2,2)])
        --print (allPairFunc [gt, ls, eq] [(5,6), (3,4), (2,2)])
        --print (allPairFunc [ls, ls, eq] [(5,6.0), (3,4.0), (2,2.0)])
