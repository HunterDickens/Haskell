doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
			then x 
			else x*2


boomBangs xs = [if x < 10 then "BOOM!" else "Bang!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]


removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float 
circumference r = 2 * pi * r 

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!!"
lucky x = "Sorry, your out of luck, bud!"


sayMe :: (Integral a) => a -> String
sayMe 1 = "One!" 
sayMe 2 = "two!" 
sayMe 3 = "three!" 
sayMe 4 = "four!" 
sayMe 5 = "five!" 
sayMe x = "Not between 1 and 5 !" 

factorialr :: (Integral a) => a -> a
factorialr 0 = 1
factorialr n = n * factorialr(n - 1)

charName :: Char -> String
charName 'a' = "Adam"
charName 'b' = "Boy"
charName 'c' = "CatDaddy"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

first :: (a, b, c) -> a 
first  (x, _ , _) = x

second :: (a, b, c) -> b
second   (_, y, _) = y

third  :: (a, b, c) -> c
third    ( _, _, z) = z

head' :: [a] -> a
head' [] = error "Cant call head on an eprty list, dummy!"
head' (x: _) = x


tell :: (Show a) => [a] -> String 
tell [] = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x 
tell ( x:y:[]) = "The list has two elements: " ++ show x ++ "and " ++ show y 
tell ( x:y:_) = "This list is long. The first two elements are: " ++ show x ++ "and" ++ show y  

lenght' :: (Num b ) => [a] -> b
lenght' [] = 0
lenght' (_ :xs) = 1 + lenght' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
captial "" = "Empty String!!"
capital all@(x:xs) = "The first letter of all " ++ all ++ " is " ++ [x]


bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
	| bmi <= 18.5 = "You need some food"
	| bmi <= 25.5 = "you are suposed to be normal"
	| bmi <= 30.0 = " Fat BOII!!" 
	| otherwise = "fat" 



initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."   
    where (f:_) = firstname  
          (l:_) = lastname 

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
	let sideArea = 2 * pi * r * h
	    topArea = pi * r^2
	in sideArea + 2 * topArea

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

maximum1 :: (Ord a) =>[a] -> a
maximum1 [] = error "maximum of empty list"
maximum1 [x] = x
maximum1 (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]  

repeat' :: a -> [a]  
repeat' x = x:repeat' x 

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip'( x: xs) (y : ys) = (x,y):zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
     | a == x   = True
     | otherwise = a `elem'` xs

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z


compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x) 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f(x : xs) (y:ys) = f x y : zipWith' f xs ys  

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

devideByTen':: (Floating a) => a -> a
devideByTen'= (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


