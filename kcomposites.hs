numOfFactors :: Integer -> [Integer]
numOfFactors  n = [x | x <- [2..div n 2 +1], n `mod` x == 0]

kcomposite :: Integer -> [Integer]
kcomposite n = [x | x <- [1..], toInteger (length (numOfFactors x)) == n ]


-- now we have this start of the hard part 
--take the length of a list and a list of k-composite (above) and check if it is two composite
isTwoComp :: Integer -> [Integer]-> Bool
isTwoComp _ [] = False
isTwoComp listLength (x:xs)
    | listLength > x = isTwoComp listLength xs
    | listLength == x = True
    | listLength < x = False

--this function will make the list two composite if it is not
makeTwoComp :: [Char] -> [Char]
makeTwoComp myWord = if x then myWord else makeTwoComp (myWord ++ ['x']) 
    where x = isTwoComp ( toInteger (length myWord)) (kcomposite 2)

-- se we know the string is now two composite number , so we can use the two factors to make a string
--so we should make a 
