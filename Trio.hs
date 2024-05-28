import Data.List

unique :: Int -> Bool
unique x 
    | nub (show x) == show x = True
    | otherwise = False
    
getHead :: String -> Char
getHead (x:xs) = x

n4 :: Int -> Int -> Bool
n4 x y
    | (union (show x) (show y) == show x) 
    && unique y = True
    | otherwise = False
    
n2 :: Int -> Int -> Bool
n2 x y
    | (union (show x) (show y) == show x) 
    && unique y 
    && getHead(show x) /= getHead(show y) = True
    | otherwise = False

generator2 :: [(String, String, String, String, String)]
generator2 = [(show a, show b, c, show d, e) |
            a <- filter unique [123..987],
            b <- filter (\x -> n2 a x) [12..98],
            c <- permutations (show a),
            d <- filter (\x -> n4 a x) [12..98],
            e <- permutations (show a)  ]

x_generator2 :: Int
x_generator2 =
    length [ t | t <- ts , t `elem` g ]
    where
    g = generator2
    ts =
        [ ("123","21","123","12","123")
        , ("162","26","261","12","621")
        , ("219","19","912","21","291")
        , ("329","92","932","32","239")
        , ("439","94","394","43","394")
        , ("549","95","945","95","945")
        , ("568","68","586","56","586")
        , ("769","67","679","97","796")
        , ("879","79","897","98","789")
        , ("987","79","789","79","789")
        ]
        
tester2 :: (String, String, String, String, String) -> Bool
tester2 (a, b, c, d, e)
    | read a - read b == read c 
    && read c - read d == read e
    && read a + read c + read e < 2000 = True
    | otherwise = False

x_tester2 :: Int
x_tester2 =
    length [ t | t <- ts , tester2 t ]
    where
    ts =
        [ ("138","01","137","50","87")
        , (" 143","01","142 ","52","90")
        , (" 171","02","169 ","79","90")
        , (" 152","03","149 ","54","95")
        , (" 159","04","155 ","61","94")
        , (" 161","05","156 ","63","93")
        , (" 182","06","176 ","80","96")
        , (" 151","07","144 ","57","87")
        , (" 165","08","157 ","64","93")
        , (" 174","09","165 ","71","94")
        ]


main :: IO()
main =
    print(filter tester2 generator2)
    --print(x_generator2)
    --print(x_tester2)
    
