module A2 where

    data Sink = Yes | No | AnInteger Int | ADouble Double | List [Sink] | AString [Char]

    instance Show Sink where
        show Yes = "True"
        show No = "False"
        show (AnInteger x) = show x
        show (ADouble x) = show x
        show (List x) = show x
        show (AString x) = show x

    class Extract a where
        asBool :: a -> Bool
        asInteger :: a -> Int
        asDouble :: a -> Double

    -- Bool, Integer, Double, Sink
    instance Extract Bool where
        asBool = id
        asInteger x | x == True = 1
                    | otherwise = 0
        asDouble x  | x == True = 1.0
                    | otherwise = 0.0

    instance Extract Int where
        asBool x    | x == 0 = False
                    | otherwise = True
        asInteger = id
        asDouble x = fromIntegral x   -- test this

    instance Extract Double where
        asBool x    | x == 0.0 = False
                    | otherwise = True
        asInteger x = truncate x
        asDouble = id
    --{-|
    instance Extract Sink where
        asBool Yes = True
        asBool No = False
        asBool (AnInteger x) = asBool x
        asBool (ADouble x) = asBool x
        asBool (List a) = (length a) /= 0
        asBool (AString a) = a /= ""

        asInteger Yes = 1
        asInteger No = 0
        asInteger (AnInteger x) = asInteger x
        asInteger (ADouble x) = asInteger x
        asInteger (List a) = asInteger $ (length a) /= 0
        asInteger (AString a) = asInteger $ a /= ""

        asDouble Yes = 1.0
        asDouble No = 0.0
        asDouble (AnInteger x) = asDouble x
        asDouble (ADouble x) = asDouble x
        asDouble (List a) = asDouble $ (length a) /= 0
        asDouble (AString a) = asDouble $ a /= ""

    ---}

    main = do
        print (Yes)
        print (asBool Yes)
        print (No)
        print (asBool No)
        print (AnInteger 0)
        print (asBool $ AnInteger 0)
        print (ADouble 0)
        print (asBool $ ADouble 0)
        print (List [])
        print (asBool $ List [])
        print (AString "False")
        print (asBool $ AString "False")
        
        