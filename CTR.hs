--helper functions needed to solve the problem:
--1. generate a list of numbers from the a = x mod n (i.e. 3 = x mod 0 = 3,6,9,12,15,18)
-- the input would be a pair of number a and n so taking 
listOfFactor :: (Integer, Integer) -> [Integer]
listOfFactor (a,n) = [a + x  | x <- [0..105], x `mod` n == 0]
--2. function which takes the numbers to find the highest i.e. 5x7x3 = 105 
sumOfAll :: [(Integer, Integer)] -> Integer
sumOfAll [] = 1
sumOfAll (x:xs) = snd x * sumOfAll xs 
-- 3. a function which compares two list to find the correct a = x mod n pair (On^2)
-- i.e. 2 = x mod 7 and 1 = x mod 5 would be 16 = x mod 35 
-- then take that new mod and generate its list with 3 = x mod 0 to get the final answer
--3.a inner loop of double for loop
smallestSharedNumber:: [Integer] -> Integer -> Bool
smallestSharedNumber myList n = n `elem` myList --if n is in the list 

--3.b outer loop of double for loop --return the [a . n]

listChecker :: [Integer] -> [Integer] -> Integer
listChecker [] _ = -1
listChecker (x:xs) l2 = if smallestSharedNumber l2 x then x else listChecker xs l2

--function take 2 tuples and generate the new one then make the new list tuple pair
equationLooper:: (Integer, Integer)-> (Integer, Integer) -> (Integer, Integer)
equationLooper (a1,n1) (a2,n2) =  (listChecker listOfFactor (a1, n1) listOfFactor (a2, n2)  , n1 * n2)