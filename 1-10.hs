import Data.List

myLast :: [a] -> a
myLast = head . reverse


myLast' :: [a] -> a
myLast' [] = error "empty list"
myLast' [x] = x
myLast' (_:xs) = myLast' xs


myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = error "one lement list" 
myButLast [x,y] = x 
myButLast (_:xs) = myButLast xs 

myButLast' [] = error "empty list"
myButLast' (x:xs) = xs !! ((length xs) - 2)


elementat :: [a] -> Int -> a
elementat (x:_) 1 = x
elementat (x:xs) n = elementat xs (n-1)


myLength :: [a]->Int
myLength [] = 0
myLength (_:xs) = 1 + myLength  xs


myLength' = sum . map (\_->1) 

myReverse :: [a]->[a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
