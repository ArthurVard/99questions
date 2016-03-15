import Data.List
{--
11. (*) Modified run-length encoding.
        Modify the result of problem 10

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
--}
data EncodingList = Single Char | Multiple Int Char deriving (Show)
encodeModified :: String -> [EncodingList]

encodeModified [] = []
encodeModified xs = map (\x->util (length x) (head x)) $ group xs
						where 
							util l c = if(l > 1) then Multiple l c
							                     else Single c  

{--
12. (**) Decode a run-length encoded list.
         Given a run-length code list generated as specified
         in problem 11. Construct its uncompressed version.

P12> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
--}

decodeModified :: [EncodingList] -> String


decodeModified = concatMap decodeHelper 
	where 
	   decodeHelper (Single c) = [c]
	   decodeHelper (Multiple n c) = replicate n c							 
				

{--
14. (*) Duplicate the elements of a list.
> dupli [1, 2, 3]
[1,1,2,2,3,3]
--}	


dupli = concatMap (replicate 2)	

dupli' [] = []
dupli' (x:xs) = x:x:dupli' xs	

{--
15. (**) Replicate the elements of a list a given number of times.
> repli "abc" 3
"aaabbbccc"
--}	

repli :: [a]->Int->[a]
repli [] _ =[]
repli (x:xs) n = (replicate n x) ++ repli xs n 

repli' xs n = concatMap (replicate n) xs

{--
16. (**) Drop every N'th element from a list.
*Main> dropEvery "abcdefghik" 3
"abdeghk"
--}

dropEvery :: [a]->Int->[a]

dropEvery xs n = [ c | (c, i)<-zip xs [1..], i `mod` n /= 0]

{--
17. (*) Split a list into two parts; 
   the length of the first part is given.

*Main> split "abcdefghik" 3
       ("abc", "defghik")
--}

split :: [a]->Int->([a], [a])
split xs n = (take n xs, drop n xs) 

{--
18. (**) Extract a slice from a list.
*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
--}

slice :: [a] -> Int -> Int -> [a]
slice xs i k = take  (k - i + 1) $ drop (i-1) xs 

slice'' xs i k = drop (i-1) $ take k xs

slice' (x:xs) i k |k <= 1 = [] 
				  |i > 1 = slice' xs (i-1) k
				  |k > 1 = x : (slice' xs i (k-1))  
				  
{--
19. (**) Rotate a list N places to the left.
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
--}      

rotate :: [a] -> Int -> [a]
rotate xs n | n > 0 = left xs n
            | n < 0 = right xs n
            where 
            	left ys m  = 