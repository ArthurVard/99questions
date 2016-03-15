{--
21. Insert an element at a given position into a list.
P21> insertAt 'X' "abcd" 2
"aXbcd"
--}

insertAt :: a -> [a] -> Int -> [a]
insertAt c [] _ = [c]
insertAt c xs 1 = c:xs
insertAt c (x:xs) n = x:insertAt c xs (n-1)

{--
22. Create a list containing all integers within a given range.
Prelude> range 4 9
[4,5,6,7,8,9]
--}

range :: Int->Int->[Int]
range a b | a == b = [a]
          | a < b = a:range (a+1) b
          | a > b = a:range (a-1) b    

{--
23. Extract a given number of randomly selected elements from a list.
Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
--}
rnd_select :: [a] -> Int -> [a]

rnd_select xs m = [x | (x, i)<-zip xs [1..], i `elem` rs]
					where 
					rs = let n = length xs - 1
					         seed  = 122
					     in 
							take m . nub . randomRs (0, n) . mkStdGen $ seed 

--prop_Rnd :: String -> Int -> String

{--
24. Lotto: Draw N different random numbers from the set 1..M.
Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
--}

diff_select :: Int->Int->[Int]

diff_select n m = take n . nub . randomRs (0, m) . mkStdGen $ (m - n)

{--
25. Generate a random permutation of the elements of a list.

Prelude>rnd_permu "abcdef"
Prelude>"badcef"
--}

rnd_permu :: [a]->[a]
rnd_permu [] = []
rnd_permu xs = [xs !! i | i<-is]
               where 
               	   n = length xs - 1
                   is = take (n+1) . nub . randomRs(0, n) . mkStdGen $ n     

{--
26. (**) Generate the combinations of K distinct objects chosen 
		 from the N elements of a list

> combinations 3 "abcdef"
["abc","abd","abe",...]
--}