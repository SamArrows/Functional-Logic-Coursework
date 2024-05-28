{- hours can be from 0 to 24
   minutes can be from 0 to 59
   month can be from 1 to 12
   day depends on month (assume non-leap year)
   
  -31 days = Jan, Mar, May, Jul, Aug, Oct, Dec
    =   1, 3, 5, 7, 8, 10, 12
  -30 days = Apr, Jun, Sep, Nov
    =   4, 6, 9, 11
  -28 days = Feb
    =   2
-if d is in [1, 3, 5, 7, 8, 10, 12]
    then c must be < 32
        - if d is in [4, 6, 9, 11] then c must be < 31
        -if d is 2 then c < 29
    
    
    -}
import Data.List    

{-
0 = hour being incremented
1 = minute being incremented
2 = day being incremented
3 = month being incremented
This function ensures that if you want the next tuple for a specific time interval that it is the correct tuple and valid --> normalisation
-}
incrementTup :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
incrementTup (a, b, c, d) 3 = (a, b, c, (d `mod` 12) + 1)
incrementTup (a, b, c, d) 2
    | valid (a, b, c+1, d) = (a, b, c+1, d)
    | otherwise = incrementTup(a, b, 1, d) 3
incrementTup (a, b, c, d) 0
    | a == 23 = incrementTup(0, b, c, d) 2
    | otherwise = (a+1, b, c, d)
incrementTup (a, b, c, d) 1
    | b == 59 = incrementTup(a, 0, c, d) 0
    | otherwise = (a, b+1, c, d)
    
--valid checks that the month is correct
valid :: (Int, Int, Int, Int) -> Bool
valid (a, b, c, d)
    | (d `elem` [1, 3, 5, 7, 8, 10, 12] && c > 31) || (d `elem` [4, 6, 9, 11] && c  > 30)  || (d == 2 && c > 29) = False
    | otherwise = True

generator1 :: [(Int, Int, Int, Int)]
generator1 = filter (valid) [(a, b, c, d) |
            a <- [0..23],
            b <- [0..59],
            c <- [1..31],
            d <- [1..12]
            ]

x_generator1 :: Int
x_generator1 =
    length [ t | t <- ts , t `elem` g ]
    where
    g = generator1
    ts =
        [ ( 2 ,15 ,14 ,11)
        , ( 4 ,31 ,27 , 9)
        , ( 6 ,47 ,10 , 8)
        , ( 9 , 3 ,23 , 6)
        , (11 ,19 , 6 , 5)
        , (13 ,35 ,19 , 3)
        , (15 ,51 , 2 , 2)
        , (18 , 6 ,16 ,12)
        , (20 ,22 ,29 ,10)
        , (22 ,38 ,11 , 9)
        ]

prime :: Int -> Bool
prime = not . factorisable 2

factorisable :: Int -> Int -> Bool
factorisable f n
    | f * f <= n = n `mod` f == 0 || factorisable (f+1) n
    | otherwise = False

--finds out how many segments are lit on a display for a single-digit number
display :: Char -> Int
display x
    | x `elem` "069" = 6
    | x == '1' = 2
    | x `elem` "235" = 5
    | x == '4' = 4
    | x == '7' = 3
    | x == '8' = 7
    | otherwise = 0

twoDig :: Int -> String
twoDig x 
    | x < 10 = "0" ++ show x
    | otherwise = show x

tupString :: (Int, Int, Int, Int) -> String
tupString (a, b, c, d) = twoDig a ++ twoDig b ++ twoDig c ++ twoDig d

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

listToString :: [String] -> String
listToString [] = []
listToString (x:xs) = 
    x ++ listToString(xs)
    
digitsUnderTen :: [Int] -> Int
digitsUnderTen [] = 0
digitsUnderTen (x:xs) 
    | x < 10 = 1 + digitsUnderTen (xs)
    | otherwise = digitsUnderTen (xs)

uniqueDigs :: [Int] -> Bool
uniqueDigs (x:xs) 
    | y == nub(y) = True
    | otherwise = False
    where y = listToString(map show (x:xs))

{- 
----only one of the numbers can be a single-digit number as a single digit number is
    represented by two numbers: a zero and the number itself; as a magic number has
    no repeating digits, it can't have multiple zeros
    
----this is accounted for with the function digitsUnderTen,
    which checks that only one of the digits is less than ten
-}
magic :: (Int, Int, Int, Int) -> Bool
magic (a, b, c, d)
    | uniqueDigs([a,b,c,d]) 
    && digitsUnderTen([a,b,c,d]) <= 1
    && prime (sumDisplay (tupString(a, b, c, d))) = True
    | otherwise = False
  
sumDisplay :: String -> Int
sumDisplay (x:xs) = sumList(map display (x:xs))

average :: [Int] -> Int
average (x:xs) = sumList (x:xs) `div` length (x:xs)

{-
Tester needs to filter tuples for which:
-the tuple is “magic”*
-a second tuple exactly one day later is also "magic"
-and just one minute on from this second tuple, the number
of lit segments on the display is the average of the number of lit segments of these two
tuples.

*A tuple (HR, MN , DY , MT) is “magic” if the displayed digits of HR, MN, DY
and MT are all different, and the total number of lit segments in the display is prime
-}

tester1 :: (Int, Int, Int, Int) -> Bool
tester1 (a, b, c, d) = 
    magic (a, b, c, d) && 
    magic (incrementTup(a, b, c, d) 2) && 
    average(
        [sumDisplay(tupString(a, b, c, d)), 
        sumDisplay(tupString(incrementTup(a, b, c, d) 2))]
        ) == sumDisplay(
            tupString(
                incrementTup(incrementTup(a, b, c, d) 2) 1
        )
        )
  
x_tester1 :: Int
x_tester1 =
    length [ t | t <- ts , tester1 t ]
    where
    ts =
        [ ( 6 ,59 ,17 ,24)
        , ( 6 ,59 ,17 ,34)
        , ( 6 ,59 ,27 ,14)
        , ( 6 ,59 ,27 ,41)
        , ( 8 ,59 ,12 ,46)
        , (16 ,59 , 7 ,24)
        , (16 ,59 , 7 ,42)
        , (16 ,59 , 7 ,43)
        , (16 ,59 ,27 ,40)
        , (18 ,59 , 2 ,46)
        ]
  
main :: IO()
main =
    --print(x_tester1)
    --print(x_generator1)
    print(filter tester1 generator1)
    