
import Data.List
{--
1.(*) Find the last element of a list.
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
--}

myLast :: [a] -> a
myLast [] = error "empty list!"
myLast (x:[]) = x -- [x] = x
myLast (_:xs) = myLast xs 


myLast' [] = error "empty list!"
myLast' xs = xs !! (length xs - 1)

{--
2.(*) Find the last but one element of a list.
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
--}

myButLast :: [a] -> a

myButLast [] = error "empty list!"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

myButLast' [] = error "empty list!"
myButLast' (x : ( _ : [])) = x
myButLast' (_:xs) = myButLast' xs


{--
3. Find the K'th element of a list. 
   The first element in the list is number 1.
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
--}

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs 0 = error "Index out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) k
  | k < 1           = error "Index out of bounds"
  | otherwise       = elementAt xs (k - 1)


elementAt'' :: [a] -> Int -> a
elementAt'' (x:_) 1  = x
elementAt'' (_:xs) i = elementAt'' xs (i - 1)
elementAt'' _ _      = error "Index out of bounds"

{--
4. (*) Find the number of elements of a list.
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
--}

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = sum [1 | _<- xs]

myLength'' ::[a] -> Int
myLength''  = sum . map (\_->1) 


{--
5. (*) Reverse a list.
Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
--}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

reverse' :: [a] -> [a]
reverse' list = reverse'' list []
  where
    reverse'' [] reversed     = reversed
    reverse'' (x:xs) reversed = reverse'' xs (x:reversed)

{--
6. (*) Find out whether a list is a palindrome. 
   A palindrome can be read forward or backward; 
   e.g. (x a m a x).
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
--} 

--isPalindrome :: [a] -> Bool
isPalindrome xs = reverse xs == xs


isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' (x:xs) = x == last xs && isPalindrome' (init xs)

{--
7. (**) Flatten a nested list structure.
We have to define a new data type, 
because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
--}
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]

flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = [] 


{--
8. (**) Eliminate consecutive duplicates of list elements.
> compress "aaaabccaadeeee"
"abcade"
--}
compress ::(Eq a)=>[a] -> [a]

compress [] =[]
compress (x:xs) = x : (compress (dropWhile (== x) xs))

compress' xs = map head  $ group xs 

{--
9. (**) Pack consecutive duplicates of list elements into sublists.
        If a list contains repeated elements they should be placed 
        in separate sublists.

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
--}

--pack :: String->[String]
pack [] = []
pack (x:xs) = (x: takeWhile (==x) xs) : pack (dropWhile (==x) xs)


{--
10. (*) Run-length encoding of a list. Use the result of 
        problem P09 to implement the so-called run-length encoding
        data compression method.

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--}

encode xs = map (\x->(length x, head x)) $ pack xs