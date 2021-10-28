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
--so make a list that holds the large factor of the list lenght

rowCreation :: [Char] -> Integer -> [Char]
rowCreation myWord n = take (fromInteger n) myWord

collumCreation :: [Char] -> [Integer] -> [[Char]]
collumCreation [] _ = []
collumCreation myWord n = [rowCreation (myWord) (sndFactor)] ++ collumCreation (drop (fromInteger sndFactor) myWord) n
    where sndFactor = last n
--use map


--it was this simple OMG well this grabs each element in a collum
combineHeads:: [[Char]] -> [Char]
combineHeads [] = []
combineHeads listOfStrings = if head listOfStrings == [] then [] else map head listOfStrings ++ combineHeads (map tail listOfStrings)
--makes the right message but gives a expetion that the tail is null


--example of mapping with getting the first element of each collum in a 2D array
col :: [[Int]] -> [Int]
col n = map head n

