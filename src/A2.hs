module A2 where

    --import Data.Typeable
    --getStaticType :: Typeable a => a -> String
    --getStaticType = show . typeOf
    
    --Create a datatype called Sink (for "everything but the kitchen sink"). 
    --It should contain constructors for True, False, Integers , Doubles, Strings (which you may have to express as [Char]), and lists of Sink objects. 
    --The constructors should be: Yes, No, AnInteger, ADouble, List, and AString. (Note, Yes and No have no parameters and encode directly the logical Bool values.)
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

    instance Extract Sink where
        asBool (AString a) = a == "True"
        asBool a = a a


    main = do
        print (Yes)
        print (asBool Yes)
        print (No)
        print (asBool No)
        print (AnInteger 3)
        print (asBool $ AnInteger 3)
        print (ADouble 4.4)
        print (asBool $ ADouble 4.4)
        print (List [Yes, No])
        print (asBool $ List [Yes, No])
        print (AString "True")
        print (asBool AString "True")
        
        