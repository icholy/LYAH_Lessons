bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30)


max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials (f:_) (l:_) = (f:". ") ++ (l:".")

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ w/h^2 | (w,h) <- xs ]

foo' :: [a] -> String
foo' xs = "The list is " ++ what xs
    where what []    = "empty"
          what [x]   = "a singleton"     
          what (x:_) = "has multiple elements"

maximum' :: (Ord a) => [a] -> a
maximum' []  = error "empty list not allowed >:("
maximum' [x] = x
maximum' (x:xs) = max x (maximum xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = [] 
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | x == a     = True
    | otherwise = elem' a xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = smaller ++ (x:larger)
    where smaller = qsort [a | a <- xs, a <= x]
	  larger  = qsort [a | a <- xs, a > x]

foo :: (Num a) => [a] -> [Float]
foo xs = let zs = zip' xs (cycle [1, 5, 10]) 
	 in map ( \ (a,b) -> a + sqrt b ) zs  


