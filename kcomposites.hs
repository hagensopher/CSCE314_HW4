numOfFactors :: Integer -> Integer
numOfFactors  n = toInteger (length [x | x <- [2..div n 2 +1], n `mod` x == 0])

kcomposite :: Integer -> [Integer]
kcomposite n = [x | x <- [1..], numOfFactors x == n ]


-- now we have this start of the hard part 
--take a string and check if it is two composite
isTwoComp :: Integer -> [Integer]-> Bool
isTwoComp _ [] = False
isTwoComp listLength (x:xs)
    | listLength > x = isTwoComp listLength xs
    | listLength == x = True
    | listLength < x = False