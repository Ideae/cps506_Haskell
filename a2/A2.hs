--Name:   Zachary Harris
--Course: CPS506, Winter 2015, Assignment #2
--Due:    2015.02.26 21:36
--Credit: This is entirely my own work.
module A2 where
    import Test.HUnit
    
    data Sink t = Yes | No | AnInteger Int | ADouble Double | List [Sink t] | AString [Char] | Other t

    instance Show (Sink t) where
        show Yes = "True"
        show No = "False"
        show (AnInteger x) = show x
        show (ADouble x) = show x
        show (List x) = show x
        show (AString x) = show x
        show (Other t) = error "Error: Other does not support show"

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
    instance Extract (Sink t) where
        asBool Yes = True
        asBool No = False
        asBool (AnInteger x) = asBool x
        asBool (ADouble x) = asBool x
        asBool (List a) = (length a) /= 0
        asBool (AString a) = a /= ""
        asBool (Other t) = error "Error: Other does not support asBool"

        asInteger Yes = 1
        asInteger No = 0
        asInteger (AnInteger x) = asInteger x
        asInteger (ADouble x) = asInteger x
        asInteger (List a) = asInteger $ (length a) /= 0
        asInteger (AString a) = asInteger $ a /= ""
        asInteger (Other t) = error "Error: Other does not support asInteger"

        asDouble Yes = 1.0
        asDouble No = 0.0
        asDouble (AnInteger x) = asDouble x
        asDouble (ADouble x) = asDouble x
        asDouble (List a) = asDouble $ (length a) /= 0
        asDouble (AString a) = asDouble $ a /= ""
        asDouble (Other t) = error "Error: Other does not support asDouble"

    instance Eq (Sink t) where
        Yes == Yes = True
        No == No = True
        Yes == No = False
        No == Yes = False
        (AnInteger x) == (AnInteger y) = x == y
        (List x) == (List y) = (length x) == (length y)
        (ADouble x) == (ADouble y) = x == y
        (AString x) == (AString y) = x == y
        (ADouble x) == (AnInteger y) = x == (asDouble y)
        (AnInteger x) == (ADouble y) = y == (asDouble x)
        _ == _ = error "Incompatible Sink types when using Eq"
        x /= y = not (x == y)

    mapFunc func [] [] = []
    mapFunc func a b | (length a) /= (length b) = []
    mapFunc func (x:y) (z:w) = (func x z : mapFunc func y w)

    mult a b = a * b

    add a b = a + b
    newf a b c = a * b - c

    instance Num (Sink t) where
        Yes + Yes = Yes
        Yes + No = Yes
        No + Yes = Yes
        No + No = No
        (AnInteger x) + (AnInteger y) = AnInteger (x + y)
        (ADouble x) + (ADouble y) = ADouble (x + y)
        (ADouble x) + (AnInteger y) = ADouble (x + (fromIntegral y))
        (AnInteger x) + (ADouble y) = ADouble (y + (fromIntegral x))
        (List x) + (List y) = List (concat [x,y])
        (AString x) + (AString y) = AString (x ++ y)
        _ + _ = error "Error: Invalid types when using the (+) operator with two Sink objects."

        Yes * Yes = Yes
        Yes * No = No
        No * Yes = No
        No * No = No
        (AnInteger x) * (AnInteger y) = AnInteger (x * y)
        (ADouble x) * (ADouble y) = ADouble (x * y)
        (ADouble x) * (AnInteger y) = ADouble (x * (fromIntegral y))
        (AnInteger x) *(ADouble y) = ADouble (y * (fromIntegral x))
        (List x) * (List y) = List (mapFunc mult x y)
        _ * _ = error "Error: while multiplying."

        negate Yes = No
        negate No = Yes
        negate (AnInteger x) = (AnInteger (negate x))
        negate (ADouble x) = (ADouble (negate x))
        negate (AString x) = error "Error: Cannot negate string"
        negate (List x) = List (map negate x)
        negate (Other t) = error "Error: Other does not support negate"

        abs Yes = Yes
        abs No = No
        abs (AnInteger x) = (AnInteger (abs x))
        abs (ADouble x) = (ADouble (abs x))
        abs (AString x) = error "Error: Cannot abs string"
        abs (List x) = List (map abs x)
        abs (Other t) = error "Error: Other does not support abs"

        signum Yes = Yes
        signum No = No
        signum (AnInteger x) = (AnInteger (signum x))
        signum (ADouble x) = (ADouble (signum x))
        signum (AString x) = error "Error: Cannot signum string"
        signum (List x) = List (map signum x)
        signum (Other t) = error "Error: Other does not support signum"
        
        fromInteger x = AnInteger (fromIntegral x)

    l1 = List [AnInteger 1, AnInteger 3]
    l2 = List [AnInteger 2, AnInteger 4]

    main = do
        assert $ show Yes == "True"
        assert $ show No == "False"
        assert $ show (AnInteger 4) == "4"
        assert $ show (ADouble 4.0) == "4.0"
        assert $ show (List [Yes, No]) == show [Yes, No]
        assert $ show (AString "hello") == show "hello"

        assert $ asBool True == True
        assert $ asInteger False == 0
        assert $ asDouble True == 1.0
        
        assert $ asBool (1 :: Int) == True
        assert $ asInteger (44 :: Int) == 44
        assert $ asDouble (2 :: Int) == 2.0

        assert $ asBool (1.0 :: Double) == True
        assert $ asInteger (22.0 :: Double) == 22
        assert $ asDouble (4.0 :: Double) == 4
        
        assert $ asBool Yes == True
        assert $ asBool No == False
        assert $ (asInteger $ AnInteger 0) == 0
        assert $ (asBool $ AnInteger 1) == True
        assert $ (asDouble $ AnInteger 0) == 0.0
        assert $ (asBool $ ADouble 0) == False
        assert $ (asBool $ List []) == False
        assert $ (asBool $ AString "True") == True

        assert $ (Yes == Yes) == True
        assert $ (No == No) == True
        assert $ (Yes == No) == False
        assert $ (No == Yes) == False
        assert $ ((AnInteger 3) == (AnInteger 4)) == False
        assert $ ((AnInteger 6) == (AnInteger 6)) == True
        assert $ (l1 == l2) == True
        assert $ (l1 == List []) == False
        assert $ ((ADouble 2.2) == (ADouble 3.3)) == False
        assert $ ((ADouble 2.2) == (ADouble 2.2)) == True
        assert $ ((AString "x") == (AString "y")) == False
        assert $ ((AString "z") == (AString "z")) == True
        assert $ ((ADouble 3.0) == (AnInteger 3)) == True
        assert $ ((AnInteger 4) == (ADouble 4.4)) == False

        assert $ Yes + Yes == Yes
        assert $ Yes + No == Yes
        assert $ No + Yes == Yes
        assert $ No + No == No
        assert $ (AnInteger 3) + (AnInteger 4) == AnInteger (7)
        assert $ (ADouble 2.2) + (ADouble 3.3) == ADouble (5.5)
        assert $ (ADouble 2.2) + (AnInteger 2) == ADouble (4.2)
        assert $ (AnInteger 4) + (ADouble 2.2) == ADouble (6.2)
        assert $ l1 + l2 == List [AnInteger 1, AnInteger 3, AnInteger 2, AnInteger 4]
        assert $ (AString "hey") + (AString "there") == AString ("heythere")

        assert $ Yes * Yes == Yes
        assert $ Yes * No == No
        assert $ No * Yes == No
        assert $ No * No == No
        assert $ (AnInteger 3) * (AnInteger 2) == AnInteger (6)
        assert $ (ADouble 1.0) * (ADouble 1.5) == ADouble (1.5)
        assert $ (ADouble 1.5) * (AnInteger 2) == ADouble (3.0)
        assert $ (AnInteger 3) * (ADouble 4.5) == ADouble (13.5)
        assert $ l1 * l2 == List [AnInteger 2, AnInteger 12]

        assert $ negate Yes == No
        assert $ negate No == Yes
        assert $ negate (AnInteger 3) == (AnInteger (-3))
        assert $ negate (ADouble 1.1) == (ADouble (-1.1))
        assert $ negate l1 == List [AnInteger (-1), AnInteger (-3)]
        
        assert $ abs Yes == Yes
        assert $ abs No == No
        assert $ abs (AnInteger (-1)) == (AnInteger 1)
        assert $ abs (ADouble (-1.1)) == (ADouble 1.1)
        assert $ (abs (List [Yes, No])) == (List [Yes, No])

        assert $ signum Yes == Yes
        assert $ signum No == No
        assert $ signum (AnInteger (-3)) == (AnInteger (-1))
        assert $ signum (ADouble (-2.2)) == (ADouble (-1.0))
        assert $ signum l1 == List [AnInteger 1, AnInteger 1]
        
        assert $ fromInteger 3 == AnInteger (3)

        print $ "All tests passed successfully."
        
